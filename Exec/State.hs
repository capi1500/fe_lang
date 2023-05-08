module Exec.State where

import Data.Map

import Control.Monad.State

import Common.Scope
import Common.Ast
import Fe.Abs (BNFC'Position)

makeExecutionState :: [String] -> ExecutionState
makeExecutionState input = ExecutionState (Global empty) [] input Nothing

putMappings :: VariableMappings -> ExecutorMonad ()
putMappings mappings = do
    ExecutionState _ variables input p <- get
    put $ ExecutionState mappings variables input p

putVariables :: [Variable] -> ExecutorMonad ()
putVariables variables = do
    ExecutionState mappings _ input p <- get
    put $ ExecutionState mappings variables input p

putInput :: [String] -> ExecutorMonad ()
putInput input = do
    ExecutionState mappings variables _ p <- get
    put $ ExecutionState mappings variables input p

putPosition :: BNFC'Position -> ExecutorMonad ()
putPosition p = do
    ExecutionState mappings variables input _ <- get
    put $ ExecutionState mappings variables input p
