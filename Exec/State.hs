module Exec.State where

import Data.Map

import Control.Monad.State

import Common.Scope
import Common.Ast

makeExecutionState :: [String] -> ExecutionState
makeExecutionState = ExecutionState (Global empty) []

putMappings :: VariableMappings -> ExecutorMonad ()
putMappings mappings = do
    ExecutionState _ variables input <- get
    put $ ExecutionState mappings variables input

putVariables :: [Variable] -> ExecutorMonad ()
putVariables variables = do
    ExecutionState mappings _ input <- get
    put $ ExecutionState mappings variables input

putInput :: [String] -> ExecutorMonad ()
putInput input = do
    ExecutionState mappings variables _ <- get
    put $ ExecutionState mappings variables input
