module Exec.Exec where

import Common.Types
import Fe.Abs
import Control.Applicative (empty)
import Common.Ast
import Control.Monad.State (StateT)
import Control.Monad.Except

type ExecutionState = Integer
type ExecutionError = String

type ExecM a = StateT ExecutionState (ExceptT ExecutionError IO) a

run :: Code' Annotation -> ExecM ()
run x = do empty
-- run (Code _ (statement:rest)) = do
--     runStatement statement
--     run (Code rest)


-- runStatement :: Annotated Statement' -> ExecM ()
-- runStatement (ExpressionStatement _ e) = do
--     fmap (print calculateExpression) e

-- calculateExpression :: Expression -> ExecM Value
-- calculateExpression (PlusExpression _ a b) = calculateExpression a + (calculateExpression b)
-- calculateExpression (LiteralExpression _ (LiteralInteger _ a)) = a
