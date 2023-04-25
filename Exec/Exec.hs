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
import Control.Monad (when)
import Exec.Utils
import Data.Maybe

class Executable a b where
    execute :: a -> ExecutorMonad b

instance Executable Code () where
    execute :: Code -> ExecutorMonad ()
    execute (Code statements) = do -- TODO: change to initialize global scope and execute main function
        traverse execute statements :: ExecutorMonad [Value]
        return ()

instance Executable Statement Value where
    execute :: Statement -> ExecutorMonad Value
    execute EmptyStatement = do
        return VUnit
    execute (TypeStatement t) = do
        throwError $ Other "Not yet implemented"
    execute (NewVariableStatement identifier id _ initialization) = do
        value <- execute initialization
        addVariable id (Variable identifier value)
        return VUnit
    execute (ExpressionStatement (TypedExpression expression _)) = do
        x <- execute expression :: ExecutorMonad Value
        liftIO $ print x
        return x

instance Executable Initialization (Maybe Value) where
    execute :: Initialization -> ExecutorMonad (Maybe Value)
    execute VarUninitialized = do
        return Nothing
    execute (VarInitialized expression) = do
        expression' <- execute expression
        return $ Just expression'

instance Executable Expression Value where
    execute :: Expression -> ExecutorMonad Value
    execute (BlockExpression statements) = do
        values <- traverse execute statements
        return $ valueOfBlock values
    execute (LiteralExpression value) = do
        return value
    execute (VariableExpression variableId) = do
        Variable _ value <- getVariable variableId
        when (isNothing value) $ throwError TypeCheckerFailed
        return (fromJust value)
    execute (I32DoubleOperatorExpression operator e1 e2) = do
        v1 <- execute e1
        v2 <- execute e2
        doNumericDoubleOperator operator v1 v2

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
doNumericDoubleOperator _ _ _ = do
    throwError TypeCheckerFailed
