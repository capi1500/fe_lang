module TypeCheck.ExpectationsUtils where

import Control.Monad.State

import Common.Types
import TypeCheck.State

setCurrentFunctionReturnType :: Type -> PreprocessorMonad ()
setCurrentFunctionReturnType t = do
    Expectations _ callParamType insideLoopExpression <- gets expectations
    putExpectations $ Expectations t callParamType insideLoopExpression

setCallParamType :: Maybe Type -> PreprocessorMonad ()
setCallParamType t = do
    Expectations currentFunctionReturnType _ insideLoopExpression <- gets expectations
    putExpectations $ Expectations currentFunctionReturnType t insideLoopExpression

setInsideLoopExpression :: Bool -> PreprocessorMonad ()
setInsideLoopExpression b = do
    Expectations currentFunctionReturnType callParamType _ <- gets expectations
    putExpectations $ Expectations currentFunctionReturnType callParamType b
