{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Exec.Exec where

import GHC.IO (catchAny, liftIO)

import Prelude hiding (print)
import Data.Maybe
import Data.Bits
import Data.Foldable (traverse_)
import Control.Exception
import Control.Applicative (empty)
import Control.Monad.Except (MonadError(..), liftIO)
import Control.Monad (when, unless)
import Control.Monad.State (get)

import Fe.Abs (BNFC'Position)

import Common.Ast
import Common.Types
import Common.Printer
import Common.AstPrinter
import Common.InternalFunctions
import Common.Utils (listGet)

import Exec.State
import Exec.Utils
import Exec.StateFunctions

class Executable a b where
    execute :: a -> ExecutorMonad b

instance Executable Code () where
    execute :: Code -> ExecutorMonad ()
    execute (Code statements) = do
        traverse_ (\(name, _, v) -> addVariable name (Variable v)) internals
        traverse_ initializeGlobalScope statements
        ExecutionState _ mainId _ _ <- get
        (_, Variable (VFunction _ _ code)) <- getVariable mainFunction
        execute code :: ExecutorMonad Value
        return ()

initializeGlobalScope :: Statement -> ExecutorMonad ()
initializeGlobalScope (ExpressionStatement _) = do return ()
initializeGlobalScope s = do
    execute s :: ExecutorMonad Value
    return ()

instance Executable Statement Value where
    execute :: Statement -> ExecutorMonad Value
    execute EmptyStatement = do
        return VUnit
    execute (TypeStatement t) = do
        throwError $ Other "Not yet implemented"
    execute (NewVariableStatement ident initialization) = do
        value <- execute initialization
        addVariable ident value
        return VUnit
    execute (NewFunctionStatement ident expression paramNames) = do
        let value = Variable $ VFunction [] paramNames expression
        addVariable ident value
        return VUnit
    execute (ExpressionStatement expression) = do
        execute expression

instance Executable Initialization Variable where
    execute :: Initialization -> ExecutorMonad Variable
    execute VarUninitialized = do
        return Uninitialized
    execute (VarInitialized expression) = do
        expression' <- execute expression >>= varValue
        return $ Variable expression'

instance Executable Expression Value where
    execute :: Expression -> ExecutorMonad Value
    execute (BlockExpression statements) = do
        inNewScope $ do
            values <- traverse (\x -> do execute x >>= varValue) statements
            deref (valueOfBlock values)
    execute (IfExpression condition onTrue maybeOnFalse) = do
        VBool bool <- execute condition >>= varValue
        if bool then do
            execute onTrue >>= varValue
        else if isJust maybeOnFalse then do
            execute (fromJust maybeOnFalse) >>= varValue
        else do
            return VUnit
    execute (WhileExpression condition block) = do
        VBool bool <- execute condition >>= varValue
        if bool then do
            x <- do {
                execute block :: ExecutorMonad Value;
                return True
            } `catchError` handler
            if x then do
                execute (WhileExpression condition block)
            else do
                return VUnit
        else do
            return VUnit
        where
            handler :: ExecutionError -> ExecutorMonad Bool
            handler Break = return False
            handler Continue = return True
            handler x = throwError x
    execute (ForExpression ident e block) = do
        VArray array <- execute e >>= varValue
        id <- addVariable ident Uninitialized
        doLoop id array
        where
            doLoop :: Pointer -> [Pointer] -> ExecutorMonad Value
            doLoop id (head:tail) = do
                Variable value <- getVariableById head
                setVariableById id value
                x <- do {
                    execute block :: ExecutorMonad Value;
                    return True
                } `catchError` handler
                if x then do
                    doLoop id tail
                else do
                    return VUnit
            doLoop _ [] = return VUnit
            handler :: ExecutionError -> ExecutorMonad Bool
            handler Break = return False
            handler Continue = return True
            handler x = throwError x
    execute (MakeArrayExpression expressions) = do
        values <- traverse (\x -> do execute x >>= varValue) expressions
        pointers <- traverse (\v -> do addTmpVariable (Variable v)) values
        return $ VArray pointers
    execute (MakeArrayDefaultsExpression e1 e2) = do
        VI32 size <- execute e1 >>= varValue
        -- print $ show size
        pointers <- traverse (\id -> do
            value <- execute e2 >>= varValue
            addTmpVariable (Variable value)
            ) [1..size]
        return $ VArray pointers
    execute (MakeClosureExpression captures params e) = do
        captures' <- traverse execute captures
        return $ VFunction captures' params e
    execute (VariableExpression ident) = do
        (pointer, variable) <- getVariable ident
        return $ VVariable pointer variable
    execute (ReferenceExpression e) = do
        VVariable pointer _ <- execute e
        let variable = Variable $ VReference pointer
        newPointer <- addTmpVariable variable
        return $ VVariable newPointer variable
    execute (DereferenceExpression e) = do
        execute e >>= deref
    execute (LiteralExpression value) = do
        return value
    execute (IndexExpression p e1 e2) = do
        VArray array <- execute e1 >>= varValue
        VI32 index <- execute e2 >>= varValue
        unless (0 <= index && index < length array) $ throwError (IndexOutOfRange p index (length array))
        let objectPointer = listGet index array
        variable <- getVariableById objectPointer
        return $ VVariable objectPointer variable
    execute (CallExpression p function_object params) = do
        putPosition p
        VFunction captures param_names code <- execute function_object >>= varValue
        params' <- traverse (\(i, e) -> do
            x <- execute e >>= varValue
            return (i, x)) (zip param_names params)
        inNewScope $ do
            traverse_ (\(paramIdent, paramValue) -> do
                addVariable paramIdent (Variable paramValue))
                params'
            traverse_ (\(ValueCapture paramIdent paramValue) -> do
                addVariable paramIdent (Variable paramValue)
                )
                captures

            do { execute code } `catchError` handler
        where
            handler :: ExecutionError -> ExecutorMonad Value
            handler (Return v) = return v
            handler x = throwError x
    execute (I32DoubleOperatorExpression p operator e1 e2) = do
        v1 <- execute e1 >>= varValue
        v2 <- execute e2 >>= varValue
        doNumericDoubleOperator p operator v1 v2
    execute (BoolDoubleOperatorExpression LazyAnd e1 e2) = do
        VBool v1 <- execute e1 >>= varValue
        if v1 then do
            execute e2 >>= varValue
        else do
            return $ VBool False
    execute (BoolDoubleOperatorExpression LazyOr e1 e2) = do
        VBool v1 <- execute e1 >>= varValue
        if not v1 then do
            execute e2 >>= varValue
        else do
            return $ VBool True
    execute (BoolDoubleOperatorExpression operator e1 e2) = do
        v1 <- execute e1 >>= varValue
        v2 <- execute e2 >>= varValue
        doBooleanDoubleOperator operator v1 v2
    execute (AssignmentExpression e1 e2) = do
        VVariable ptr _ <- execute e1
        v <- execute e2 >>= varValue
        setVariableById ptr v
        return VUnit
    execute (UnaryMinusExpression e) = do
        VI32 v <- execute e >>= varValue
        return $ VI32 (-v)
    execute (UnaryNegationExpression e) = do
        VBool v <- execute e >>= varValue
        return $ VBool (not v)
    execute (RangeExpression e1 e2) = do
        VI32 v1 <- execute e1 >>= varValue
        VI32 v2 <- execute e2 >>= varValue
        let values = if v1 < v2 then
                [VI32 i | i <- [v1..v2]]
            else
                reverse ([VI32 i | i <- [v2..v1]])

        pointers <- traverse (\v -> do addTmpVariable (Variable v)) values
        return $ VArray pointers
    execute (ReturnExpression e) = do
        v <- execute e >>= varValue
        throwError $ Return v
    execute BreakExpression = do
        throwError Break
    execute ContinueExpression = do
        throwError Continue
    execute (InternalExpression f) = do
        f

instance Executable Capture ValueCapture where
    execute (Capture name CMNone) = do
        VVariable _ (Variable v) <- execute (VariableExpression name)
        return $ ValueCapture name v
    execute (Capture name (CMRef _)) = do
        VVariable _ (Variable v) <- execute (ReferenceExpression (VariableExpression name))
        return $ ValueCapture name v

doBooleanDoubleOperator :: BooleanDoubleOperator -> Value -> Value -> ExecutorMonad Value
doBooleanDoubleOperator Equals v1 v2 = do
    v1' <- deref v1
    v2' <- deref v2
    x <- equals v1' v2'
    return $ VBool x
doBooleanDoubleOperator Greater v1 v2 = do
    VI32 v1' <- deref v1
    VI32 v2' <- deref v2
    return $ VBool (v1' > v2')
doBooleanDoubleOperator Smaller v1 v2 = do
    VI32 v1' <- deref v1
    VI32 v2' <- deref v2
    return $ VBool (v1' < v2')
doBooleanDoubleOperator op v1 v2 = do
    throwError $ TypeCheckerFailed (codePrint 0 v1 ++ " " ++ show op ++ " " ++ codePrint 0 v2)

equals :: Value -> Value -> ExecutorMonad Bool
equals (VI32 a) (VI32 b) = do return $ a == b
equals (VChar a) (VChar b) = do return $  a == b
equals (VBool a) (VBool b) = do return $  a == b
equals VUnit VUnit = do return True
equals (VStruct a) (VStruct b) = do
    throwError $ Other "Not yet implemented: compare of structs"
equals (VVariant tag1 a) (VVariant tag2 b) = do
    throwError $ Other "Not yet implemented: compare of variants"
equals VFunction {} VFunction {} = do
    throwError $ TypeCheckerFailed "Cannot compare function values directly"
equals (VArray a) (VArray b) = do
    if length a /= length b then do
        return False
    else do
        values1 <- traverse getValueById a
        values2 <- traverse getValueById b
        out <- traverse (uncurry equals) (zip values1 values2)
        return $ and out
equals (VReference a) (VReference b) = do
    return $ a == b
equals _ _ = do return False

doNumericDoubleOperator :: BNFC'Position -> NumericDoubleOperator -> Value -> Value -> ExecutorMonad Value
doNumericDoubleOperator _ Plus (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 + v2)
doNumericDoubleOperator _ Minus (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 - v2)
doNumericDoubleOperator _ Multiply (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 * v2)
doNumericDoubleOperator p Divide (VI32 v1) (VI32 v2) = do
    when (v2 == 0) $ throwError (DivisionByZero p)
    return $ VI32 (div v1 v2)
doNumericDoubleOperator p Modulo (VI32 v1) (VI32 v2) = do
    when (v2 == 0) $ throwError (DivisionByZero p)
    return $ VI32 (mod v1 v2)
doNumericDoubleOperator p LShift (VI32 v1) (VI32 v2) = do
    when (v2 < 0 || v2 >= finiteBitSize v2) $ throwError (ShiftInvalidArgument p v2)
    return $ VI32 (shiftL v1 v2)
doNumericDoubleOperator p RShift (VI32 v1) (VI32 v2) = do
    when (v2 < 0 || v2 >= finiteBitSize v2) $ throwError (ShiftInvalidArgument p v2)
    return $ VI32 (shiftR v1 v2)
doNumericDoubleOperator _ BitOr (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 .|. v2)
doNumericDoubleOperator _ BitXor (VI32 v1) (VI32 v2) = do
    return $ VI32 (xor v1 v2)
doNumericDoubleOperator _ BitAnd (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 .&. v2)
doNumericDoubleOperator _ op v1 v2 = do
    throwError $ TypeCheckerFailed (codePrint 0 v1 ++ " " ++ show op ++ " " ++ codePrint 0 v2)
