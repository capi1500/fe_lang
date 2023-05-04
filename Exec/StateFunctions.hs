module Exec.StateFunctions where

import Prelude hiding (lookup)
import Data.Map
import Control.Monad.State

import Fe.Abs (Ident)

import Common.Utils
import Common.Scope
import Exec.State
import Data.Maybe
import Common.Ast

addVariable :: Ident -> Variable -> ExecutorMonad Pointer
addVariable ident variable = do
    ExecutionState mappings variables <- get
    let id = length variables
    put $ ExecutionState (helper mappings id) (listPushBack variable variables)
    return id
  where
    helper (Global map) id = Global $ insert ident id map
    helper (Local parent map) id = Local parent (insert ident id map)

addTmpVariable :: Variable -> ExecutorMonad Pointer
addTmpVariable variable = do
    ExecutionState mappings variables <- get
    let id = length variables
    put $ ExecutionState mappings (listPushBack variable variables)
    return id

getVariable :: Ident -> ExecutorMonad (Pointer, Variable)
getVariable ident = do
    ExecutionState mappings variables <- get
    let id = helper mappings ident
    return (id, listGet id variables)
  where
    helper :: VariableMappings -> Ident -> VariableId
    helper (Global map) ident = fromJust (lookup ident map)
    helper (Local parent map) ident =
        let maybeId = lookup ident map in
        fromMaybe (helper parent ident) maybeId

getVariableById :: Pointer -> ExecutorMonad Variable
getVariableById id = do
  variables <- gets variables
  return $ listGet id variables

setVariableById :: Pointer -> Value -> ExecutorMonad ()
setVariableById id value = do
  variables <- gets variables
  putVariables (listSet id (Variable value) variables)

removeVariable :: Ident -> ExecutorMonad ()
removeVariable ident = do
    ExecutionState mappings variables <- get
    let id = length variables
    put $ ExecutionState (helper mappings id) variables
  where
    helper (Global map) id = Global $ insert ident id map
    helper (Local parent map) id = Local parent (insert ident id map)

inNewScope :: ExecutorMonad a -> ExecutorMonad a
inNewScope f = do
    state <- get
    let mappings = variableMappings state  -- record current state
    putMappings $ Local mappings empty
    ret' <- f
    put state
    return ret'

inNewFrame :: ExecutorMonad a -> ExecutorMonad a
inNewFrame f = do
    state <- get
    let mappings = variableMappings state  -- record current state
    putMappings $ Local mappings empty
    ret' <- f
    put state
    return ret'


makeNewFrame :: VariableMappings -> VariableMappings
makeNewFrame (Global global) = Local (Global global) empty
makeNewFrame (Local parent _) = makeNewFrame parent
