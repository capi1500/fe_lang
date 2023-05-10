module TypeCheck.ContextUtils where

import Control.Monad.State

import Common.Types
import TypeCheck.State

setCurrentFunction :: Type -> PreprocessorMonad ()
setCurrentFunction t = do
    Context _ insideLoopExpression <- gets context
    putContext $ Context t insideLoopExpression

setInsideLoopExpression :: Bool -> PreprocessorMonad ()
setInsideLoopExpression b = do
    Context currentFunction _ <- gets context
    putContext $ Context currentFunction b
