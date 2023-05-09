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
    reproduceWithPersistent (variables state) state
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
    variables <- gets variables
    reproduceWithPersistent variables state
    return ret

reproduceWithPersistent :: Variables -> PreprocessorState -> PreprocessorMonad ()
reproduceWithPersistent variables state = do
    let map = getMap variables
    traverse_ moveOutById (sortBy (flip compare) (elems map))

    warnings <- gets warnings
    LifetimeState _ id <- gets lifetimeState

    put state
    putVariables variables

    LifetimeState lifetime _ <- gets lifetimeState
    putWarnings warnings
    putLifetimeState (LifetimeState lifetime id)
  where
    getMap (Variables (Global map) _) = map
    getMap (Variables (Local _ map) _) = map

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
