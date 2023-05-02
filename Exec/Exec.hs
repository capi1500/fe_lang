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

-- class Executable a b where
--     execute :: a -> ExecutorMonad b

-- instance Executable Code () where
--     execute :: Code -> ExecutorMonad ()
--     execute (Code statements) = do
--         traverse initializeGlobalScope statements :: ExecutorMonad [()]
--         ExecutionState _ mainId <- get
--         Variable maybeMain <- getVariable mainId
--         let VFunction _ code = fromJust maybeMain
--         x <- execute code :: ExecutorMonad Value
--         liftIO $ print x
--         return ()

-- initializeGlobalScope :: Statement -> ExecutorMonad ()
-- initializeGlobalScope (ExpressionStatement _) = do return ()
-- initializeGlobalScope s = do
--     execute s :: ExecutorMonad Value
--     return ()

-- instance Executable Statement Value where
--     execute :: Statement -> ExecutorMonad Value
--     execute EmptyStatement = do
--         return VUnit
--     execute (TypeStatement t) = do
--         throwError $ Other "Not yet implemented"
--     execute (NewVariableStatement identifier id initialization) = do
--         value <- execute initialization
--         addVariable id (Variable value)
--         return VUnit
--     execute (NewFunctionStatement identifier id expression paramIds) = do
--         let value = VFunction paramIds expression
--         addVariable id (Variable (Just value))
--         return VUnit
--     execute (ExpressionStatement (TypedExpression expression _)) = do
--         execute expression

-- instance Executable Initialization (Maybe Value) where
--     execute :: Initialization -> ExecutorMonad (Maybe Value)
--     execute VarUninitialized = do
--         return Nothing
--     execute (VarInitialized expression) = do
--         expression' <- execute expression
--         return $ Just expression'

-- instance Executable Expression Value where
--     execute :: Expression -> ExecutorMonad Value
--     execute (BlockExpression statements) = do
--         values <- traverse execute statements
--         return $ valueOfBlock values
--     execute (IfExpression condition onTrue maybeOnFalse) = do
--         VBool bool <- execute condition
--         if bool then do
--             execute onTrue
--         else if isJust maybeOnFalse then do
--             execute (fromJust maybeOnFalse)
--         else do
--             return VUnit
--     execute (LiteralExpression value) = do
--         return value
--     execute (MoveExpression (VMovedValue variableId)) = do
--         Variable (Just value) <- getVariable variableId
--         unless (isPrimitiveValue value) $ removeVariable variableId
--         return value
--     execute (ReferenceExpression value) = do
--         return value
--     execute (CallExpression function_object params) = do
--         VReference referenceType functionId <- execute function_object
--         Variable (Just (VFunction paramsIds code)) <- getVariable functionId
--         traverse_ (\(paramExpression, paramId) -> do
--             paramValue <- execute paramExpression
--             addVariable paramId (Variable (Just paramValue)))
--             (zip params paramsIds)
--         ExecutionState variables _ <- get
--         liftIO $ print variables
--         returnValue <- execute code
--         traverse_ (\paramId -> do
--             removeVariable paramId)
--             paramsIds
--         return returnValue
--     execute (I32DoubleOperatorExpression operator e1 e2) = do
--         v1 <- execute e1
--         v2 <- execute e2
--         doNumericDoubleOperator operator v1 v2
--     execute (BoolDoubleOperatorExpression operator e1 e2) = do
--         v1 <- execute e1
--         v2 <- execute e2
--         doBooleanDoubleOperator operator v1 v2
--     execute _ = do
--         throwError TypeCheckerFailed

-- doBooleanDoubleOperator :: BooleanDoubleOperator -> Value -> Value -> ExecutorMonad Value
-- doBooleanDoubleOperator Equals v1 v2 = do
--     return $ VBool (v1 == v2)
-- doBooleanDoubleOperator NotEquals v1 v2 = do
--     return $ VBool (v1 == v2)
-- doBooleanDoubleOperator Greater (VI32 v1) (VI32 v2) = do
--     return $ VBool (v1 > v2)
-- doBooleanDoubleOperator Smaller (VI32 v1) (VI32 v2) = do
--     return $ VBool (v1 < v2)
-- doBooleanDoubleOperator GreaterEquals (VI32 v1) (VI32 v2) = do
--     return $ VBool (v1 >= v2)
-- doBooleanDoubleOperator SmallerEquals (VI32 v1) (VI32 v2) = do
--     return $ VBool (v1 <= v2)
-- doBooleanDoubleOperator LazyAnd (VBool v1) (VBool v2) = do
--     return $ VBool (v1 && v2)
-- doBooleanDoubleOperator LazyOr (VBool v1) (VBool v2) = do
--     return $ VBool (v1 || v2)
-- doBooleanDoubleOperator _ _ _ = do
--     throwError TypeCheckerFailed

-- doNumericDoubleOperator :: NumericDoubleOperator -> Value -> Value -> ExecutorMonad Value
-- doNumericDoubleOperator Plus (VI32 v1) (VI32 v2) = do
--     return $ VI32 (v1 + v2)
-- doNumericDoubleOperator Minus (VI32 v1) (VI32 v2) = do
--     return $ VI32 (v1 - v2)
-- doNumericDoubleOperator Multiply (VI32 v1) (VI32 v2) = do
--     return $ VI32 (v1 * v2)
-- doNumericDoubleOperator Divide (VI32 v1) (VI32 v2) = do
--     when (v2 == 0) $ throwError (DivisionByZero (Just (0, 0))) -- TODO: fix error handling
--     return $ VI32 (div v1 v2)
-- doNumericDoubleOperator Modulo (VI32 v1) (VI32 v2) = do
--     when (v2 == 0) $ throwError (DivisionByZero (Just (0, 0))) -- TODO: fix error handling
--     return $ VI32 (mod v1 v2)
-- doNumericDoubleOperator LShift (VI32 v1) (VI32 v2) = do
--     when (v2 < 0 || v2 >= finiteBitSize v2) $ throwError (ShiftInvalidArgument v2) -- TODO: fix error handling
--     return $ VI32 (shiftL v1 v2)
-- doNumericDoubleOperator RShift (VI32 v1) (VI32 v2) = do
--     when (v2 < 0 || v2 >= finiteBitSize v2) $ throwError (ShiftInvalidArgument v2) -- TODO: fix error handling
--     return $ VI32 (shiftR v1 v2)
-- doNumericDoubleOperator BitOr (VI32 v1) (VI32 v2) = do
--     return $ VI32 (v1 .|. v2)
-- doNumericDoubleOperator BitXor (VI32 v1) (VI32 v2) = do
--     return $ VI32 (xor v1 v2)
-- doNumericDoubleOperator BitAnd (VI32 v1) (VI32 v2) = do
--     return $ VI32 (v1 .&. v2)
-- doNumericDoubleOperator _ _ _ = do
--     throwError TypeCheckerFailed
