module TypeCheck.ExpectationsUtils where

import Control.Monad.State

import Common.Types
import TypeCheck.State

setCurrentFunctionReturnType :: Type -> PreprocessorMonad ()
setCurrentFunctionReturnType t = do
    Context _ insideLoopExpression <- gets context
    putContext $ Context t insideLoopExpression

setInsideLoopExpression :: Bool -> PreprocessorMonad ()
setInsideLoopExpression b = do
    Context currentFunctionReturnType _ <- gets context
    putContext $ Context currentFunctionReturnType b
