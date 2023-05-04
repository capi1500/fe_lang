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

class Executable a b where
    execute :: a -> ExecutorMonad b

instance Executable Code () where
    execute :: Code -> ExecutorMonad ()
    execute (Code statements) = do
        traverse_ initializeGlobalScope statements
        ExecutionState _ mainId <- get
        (_, Variable (VFunction _ code)) <- getVariable mainFunctionName
        x <- execute code :: ExecutorMonad Value
        liftIO $ print x
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
    execute (NewVariableStatement ident isRef initialization) = do
        value <- execute initialization >>= derefVariable isRef
        addVariable ident value
        return VUnit
    execute (NewFunctionStatement ident expression paramIdents) = do
        let value = Variable $ VFunction [] expression
        addVariable ident value
        return VUnit
    execute (ExpressionStatement (TypedExpression expression _ _)) = do
        execute expression

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
    execute (MakeArrayExpression values) = do
        pointers <- traverse (\v -> do addTmpVariable (Variable v)) values
        return $ VArray (length values) pointers
    execute (VariableExpression ident) = do
        (pointer, _) <- getVariable ident
        return $ VReference pointer
    execute (LiteralExpression value) = do
        return value
    execute (CallExpression function_object params) = do
        VFunction _ code <- execute function_object >>= deref
        params' <- traverse (\(i, isRef, e) -> do
            x <- derefConditionally isRef (execute e)
            return (i, x)) params
        inNewScope $ do
            traverse_ (\(paramIdent, paramValue) -> do
                addVariable paramIdent (Variable paramValue))
                params'
            execute code
    execute (I32DoubleOperatorExpression operator e1 e2) = do
        v1 <- execute e1 >>= deref
        v2 <- execute e2 >>= deref
        doNumericDoubleOperator operator v1 v2
    execute (BoolDoubleOperatorExpression operator e1 e2) = do
        v1 <- execute e1 >>= deref
        v2 <- execute e2 >>= deref
        doBooleanDoubleOperator operator v1 v2
    -- execute (AssignmentExpression isRef e1 e2) = do
    --     v1 <- execute e1
    --     v2 <- execute e2
    --     return v1 -- TODO
    execute _ = do
        throwError $ Other "Not yet implemented"

doBooleanDoubleOperator :: BooleanDoubleOperator -> Value -> Value -> ExecutorMonad Value
doBooleanDoubleOperator Equals v1 v2 = do
    return $ VBool (v1 == v2)
doBooleanDoubleOperator Greater (VI32 v1) (VI32 v2) = do
    return $ VBool (v1 > v2)
doBooleanDoubleOperator Smaller (VI32 v1) (VI32 v2) = do
    return $ VBool (v1 < v2)
doBooleanDoubleOperator GreaterEquals (VI32 v1) (VI32 v2) = do
    return $ VBool (v1 >= v2)
doBooleanDoubleOperator SmallerEquals (VI32 v1) (VI32 v2) = do
    return $ VBool (v1 <= v2)
doBooleanDoubleOperator LazyAnd (VBool v1) (VBool v2) = do
    return $ VBool (v1 && v2)
doBooleanDoubleOperator LazyOr (VBool v1) (VBool v2) = do
    return $ VBool (v1 || v2)
doBooleanDoubleOperator op v1 v2 = do
    throwError $ TypeCheckerFailed (show v1 ++ " " ++ show op ++ " " ++ show v2)

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
    throwError $ TypeCheckerFailed (show v1 ++ " " ++ show op ++ " " ++ show v2)
