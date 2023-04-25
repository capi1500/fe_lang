module Exec.State where

import Control.Monad.State (StateT, get, put)
import Control.Monad.Except
import Exec.Error
import Common.Types
import Common.Utils
import Common.Ast
import Data.Array (Array, array)
import TypeCheck.State (getVariable)


data Variable = Variable Identifier (Maybe Value) | VariableUndefined
  deriving (Eq, Ord, Show, Read)

data ExecutionState = ExecutionState [Variable] VariableId
  deriving (Eq, Ord, Show, Read)

type ExecutorMonad a = StateT ExecutionState (ExceptT ExecutionError IO) a

makeExecutionState :: Int -> VariableId -> ExecutionState
makeExecutionState stackSize = ExecutionState
        [VariableUndefined | i <- [0..stackSize]]

addVariable :: VariableId -> Variable -> ExecutorMonad ()
addVariable id variable = do
    ExecutionState stack mainId <- get
    put $ ExecutionState (listSet id variable stack) mainId
    return ()

getVariable :: VariableId -> ExecutorMonad Variable
getVariable id = do
    ExecutionState stack mainId <- get
    return $ listGet id stack
