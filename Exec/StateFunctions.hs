module Exec.StateFunctions where

import Prelude hiding (lookup)
import Data.Map
import Control.Monad.State

import Common.Utils
import Common.Scope
import Exec.State
import Data.Maybe
import Common.Ast

addVariable :: Identifier -> Variable -> ExecutorMonad Pointer
addVariable ident variable = do
    ExecutionState mappings variables input p <- get
    let id = length variables
    put $ ExecutionState (helper mappings id) (listPushBack variable variables) input p
    return id
  where
    helper (Global map) id = Global $ insert ident id map
    helper (Local parent map) id = Local parent (insert ident id map)

addTmpVariable :: Variable -> ExecutorMonad Pointer
addTmpVariable variable = do
    variables <- gets variables
    let id = length variables
    putVariables $ listPushBack variable variables
    return id

getVariable :: Identifier -> ExecutorMonad (Pointer, Variable)
getVariable ident = do
    ExecutionState mappings variables _ _ <- get
    let id = helper mappings ident
    return (id, listGet id variables)
  where
    helper :: VariableMappings -> Identifier -> Pointer
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

removeVariable :: Identifier -> ExecutorMonad ()
removeVariable ident = do
    ExecutionState mappings variables input p <- get
    let id = length variables
    put $ ExecutionState (helper mappings id) variables input p
  where
    helper (Global map) id = Global $ insert ident id map
    helper (Local parent map) id = Local parent (insert ident id map)

inNewScope :: ExecutorMonad a -> ExecutorMonad a
inNewScope f = do
    state <- get
    let mappings = variableMappings state  -- record current state
    putMappings $ Local mappings empty
    ret' <- f
    putMappings mappings
    return ret'

inNewFrame :: ExecutorMonad a -> ExecutorMonad a
inNewFrame f = do
    state <- get
    let mappings = variableMappings state  -- record current state
    putMappings $ Local mappings empty
    ret' <- f
    putMappings mappings
    return ret'


makeNewFrame :: VariableMappings -> VariableMappings
makeNewFrame (Global global) = Local (Global global) empty
makeNewFrame (Local parent _) = makeNewFrame parent
