{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Exec.Exec where

import Common.Types
import Control.Applicative (empty)
import Common.Ast
import Exec.State
import Exec.Error
import Control.Monad.Except (MonadError(..), liftIO)
import Data.Bits
import Control.Monad (when, unless)
import Exec.Utils
import Data.Maybe
import Control.Monad.State (get)
import Data.Foldable (traverse_)
import Exec.StateFunctions
import Common.Printer
import Common.AstPrinter
import Common.InternalFunctions
import Common.Utils (listGet)

class Executable a b where
    execute :: a -> ExecutorMonad b

instance Executable Code () where
    execute :: Code -> ExecutorMonad ()
    execute (Code statements) = do
        traverse_ (\(name, _, v) -> addVariable name (Variable v)) internals
        traverse_ initializeGlobalScope statements
        ExecutionState _ mainId _ <- get
        (_, Variable (VFunction _ code)) <- getVariable mainFunction
        x <- execute code :: ExecutorMonad Value
        liftIO (putStrLn $ codePrint 0 x)
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
        printLocalScope
        return VUnit
    execute (NewFunctionStatement ident expression paramIdents) = do
        let value = Variable $ VFunction paramIdents expression
        addVariable ident value
        return VUnit
    execute (ExpressionStatement expression) = do
        value <- execute expression
        printLocalScope
        return value

instance Executable Initialization Variable where
    execute :: Initialization -> ExecutorMonad Variable
    execute VarUninitialized = do
        return Uninitialized
    execute (VarInitialized expression) = do
        expression' <- execute expression
        return $ Variable expression'

instance Executable Expression Value where
    execute :: Expression -> ExecutorMonad Value
    execute (BlockExpression statements) = do
        values <- traverse execute statements
        deref (valueOfBlock values)
    execute (IfExpression condition onTrue maybeOnFalse) = do
        VBool bool <- execute condition
        if bool then do
            execute onTrue
        else if isJust maybeOnFalse then do
            execute (fromJust maybeOnFalse)
        else do
            return VUnit
    execute (MakeArrayExpression expressions) = do
        values <- traverse execute expressions
        pointers <- traverse (\v -> do addTmpVariable (Variable v)) values
        return $ VArray pointers
    execute (MakeArrayDefaultsExpression e1 e2) = do
        VI32 size <- execute e1
        value <- execute e2
        pointers <- traverse (\id -> do addTmpVariable (Variable value)) [1..size]
        return $ VArray pointers
    execute (VariableExpression ident) = do
        (_, Variable value) <- getVariable ident
        return value
    execute (ReferenceExpression ident) = do
        (pointer, _) <- getVariable ident
        return $ VReference pointer
    execute (DereferenceExpression e) = do
        execute e >>= deref
    execute (LiteralExpression value) = do
        return value
    execute (IndexExpression e1 e2) = do
        VArray array <- execute e1 >>= deref
        VI32 index <- execute e2
        return $ VReference (listGet index array)
    execute (CallExpression function_object params) = do
        VFunction param_names code <- execute function_object >>= deref
        params' <- traverse (\(i, e) -> do
            x <- execute e
            return (i, x)) (zip param_names params)
        inNewScope $ do
            traverse_ (\(paramIdent, paramValue) -> do
                addVariable paramIdent (Variable paramValue))
                params'
            execute code
    execute (I32DoubleOperatorExpression operator e1 e2) = do
        v1 <- execute e1
        v2 <- execute e2
        doNumericDoubleOperator operator v1 v2
    execute (BoolDoubleOperatorExpression operator e1 e2) = do
        v1 <- execute e1
        v2 <- execute e2
        doBooleanDoubleOperator operator v1 v2
    execute (AssignmentExpression e1 e2) = do
        VReference ptr <- execute e1
        v <- execute e2
        setVariableById ptr v
        return $ VReference ptr
    execute (UnaryMinusExpression e) = do
        VI32 v <- execute e
        return $ VI32 (-v)
    execute (UnaryNegationExpression e) = do
        VBool v <- execute e
        return $ VBool (not v)
    execute (InternalExpression f) = do
        f

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
doBooleanDoubleOperator LazyAnd (VBool v1) (VBool v2) = do
    return $ VBool (v1 && v2)
doBooleanDoubleOperator LazyOr (VBool v1) (VBool v2) = do
    return $ VBool (v1 || v2)
doBooleanDoubleOperator op v1 v2 = do
    throwError $ TypeCheckerFailed (codePrint 0 v1 ++ " " ++ show op ++ " " ++ codePrint 0 v2)

equals :: Value -> Value -> ExecutorMonad Bool
equals (VI32 a) (VI32 b) = do return $ a == b
equals (VChar a) (VChar b) = do return $  a == b
equals (VBool a) (VBool b) = do return $  a == b
equals VUnit VUnit = do return True
equals (VStruct a) (VStruct b) = do
    throwError $ Other "Not yet implemented: cmp of structs"
equals (VVariant tag1 a) (VVariant tag2 b) = do
    throwError $ Other "Not yet implemented: cmp of variants"
equals (VFunction _ _) (VFunction _ _) = do
    throwError $ TypeCheckerFailed "Cannot compare function values directly"
equals (VArray a) (VArray b) = do
    if length a /= length b then do
        return False
    else do
        out <- traverse (uncurry equals) (zip (fmap VReference a) (fmap VReference b))
        return $ and out
equals (VReference a) (VReference b) = do
    if a == b then do
        return True
    else do
        Variable a' <- getVariableById a
        Variable b' <- getVariableById b
        equals a' b'
equals _ _ = do return False

doNumericDoubleOperator :: NumericDoubleOperator -> Value -> Value -> ExecutorMonad Value
doNumericDoubleOperator Plus (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 + v2)
doNumericDoubleOperator Minus (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 - v2)
doNumericDoubleOperator Multiply (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 * v2)
doNumericDoubleOperator Divide (VI32 v1) (VI32 v2) = do
    when (v2 == 0) $ throwError (DivisionByZero (Just (0, 0))) -- TODO: fix error handling
    return $ VI32 (div v1 v2)
doNumericDoubleOperator Modulo (VI32 v1) (VI32 v2) = do
    when (v2 == 0) $ throwError (DivisionByZero (Just (0, 0))) -- TODO: fix error handling
    return $ VI32 (mod v1 v2)
doNumericDoubleOperator LShift (VI32 v1) (VI32 v2) = do
    when (v2 < 0 || v2 >= finiteBitSize v2) $ throwError (ShiftInvalidArgument v2) -- TODO: fix error handling
    return $ VI32 (shiftL v1 v2)
doNumericDoubleOperator RShift (VI32 v1) (VI32 v2) = do
    when (v2 < 0 || v2 >= finiteBitSize v2) $ throwError (ShiftInvalidArgument v2) -- TODO: fix error handling
    return $ VI32 (shiftR v1 v2)
doNumericDoubleOperator BitOr (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 .|. v2)
doNumericDoubleOperator BitXor (VI32 v1) (VI32 v2) = do
    return $ VI32 (xor v1 v2)
doNumericDoubleOperator BitAnd (VI32 v1) (VI32 v2) = do
    return $ VI32 (v1 .&. v2)
doNumericDoubleOperator op v1 v2 = do
    throwError $ TypeCheckerFailed (codePrint 0 v1 ++ " " ++ show op ++ " " ++ codePrint 0 v2)
