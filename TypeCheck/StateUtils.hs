module TypeCheck.StateUtils where

import Data.List
import Data.Foldable hiding (toList)
import Data.Map
import Control.Monad.State

import Fe.Abs (BNFC'Position)

import Common.Scope
import Common.Printer
import Common.Types

import TypeCheck.LifetimeUtils
import TypeCheck.VariablesUtils
import TypeCheck.ExpressionEval
import TypeCheck.Error
import TypeCheck.State
import TypeCheck.Variable
import TypeCheck.BorrowCheckerUtils
import TypeCheck.Printer
import Common.Utils

makeNewFrame :: Variables -> Variables
makeNewFrame (Variables (Global global) variables) = Variables (Local (Global global) empty) variables
makeNewFrame (Variables (Local parent _) variables) = makeNewFrame (Variables parent variables)

inNewFrame :: PreprocessorMonad a -> PreprocessorMonad a
inNewFrame f = do
    p <- gets position
    state <- get
    lifetime <- forkLifetime staticLifetime
    saveLifetime lifetime
    putVariables $ makeNewFrame (variables state)
    putTypeDefinitions $ Local (typeDefinitions state) empty
    ret' <- f
    putPosition p
    reproduceWithPersistent state
    return ret'

inNewScope :: PreprocessorMonad a -> PreprocessorMonad a
inNewScope f = do
    p <- gets position
    state <- get -- record current state
    updateLifetime
    let Variables mappings container = variables state
    putVariables $ Variables (Local mappings empty) container
    putTypeDefinitions $ Local (typeDefinitions state) empty
    ret <- f
    putPosition p
    reproduceWithPersistent state
    return ret

reproduceWithPersistent :: PreprocessorState -> PreprocessorMonad ()
reproduceWithPersistent state = do
    let Variables oldMappings oldVariables = variables state
    Variables newMappings newVariables <- gets variables
    let map = getMap newMappings
    traverse_ moveOutById (sortBy (flip compare) (elems map))

    warnings <- gets warnings
    LifetimeState _ id <- gets lifetimeState

    put state

    let variablesToReproduce = fmap snd (zip oldVariables newVariables)
    putVariables $ Variables oldMappings variablesToReproduce

    LifetimeState lifetime _ <- gets lifetimeState
    putWarnings warnings
    putLifetimeState (LifetimeState lifetime id)
  where
    getMap (Global map) = map
    getMap (Local _ map) = map

withinContext :: PreprocessorMonad a -> PreprocessorMonad a
withinContext f = do
    context <- gets expressionContext
    ret <- f
    putExpressionContext context
    return ret

endStatement :: PreprocessorMonad ()
endStatement = do
    toDrop <- gets toDropAtStatementEnd
    traverse_ moveOutById toDrop
    clearVariablesToDrop
