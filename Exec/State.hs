module Exec.State where

import Data.Array
import Control.Monad.State
import Control.Monad.Except

import Exec.Error

import Common.Types
import Common.Utils
import Common.Ast
import Common.Scope
import Data.Map

type VariableId = Int

data Variable = Variable Value | Uninitialized
  deriving (Eq, Ord, Show, Read)

type VariableMappings = Scope (Map Identifier VariableId)
data ExecutionState = ExecutionState {
    variableMappings :: VariableMappings,
    variables :: [Variable]
} deriving (Eq, Ord, Show, Read)

type ExecutorMonad a = StateT ExecutionState (ExceptT ExecutionError IO) a

makeExecutionState :: ExecutionState
makeExecutionState  = ExecutionState (Global empty) []

putMappings :: VariableMappings -> ExecutorMonad ()
putMappings mappings = do
    ExecutionState _ variables <- get
    put $ ExecutionState mappings variables

putVariables :: [Variable] -> ExecutorMonad ()
putVariables variables = do
    ExecutionState mappings _ <- get
    put $ ExecutionState mappings variables