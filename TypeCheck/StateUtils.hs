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
    variables <- gets variables
    let Variables (Local _ map) _ = variables
    traverse_ moveOutById (sortBy (flip compare) (elems map))

    warnings <- gets warnings
    LifetimeState _ id <- gets lifetimeState

    put state

    LifetimeState lifetime _ <- gets lifetimeState
    putWarnings warnings
    putLifetimeState (LifetimeState lifetime id)

withinContext :: PreprocessorMonad a -> PreprocessorMonad a
withinContext f = do
    context <- gets context
    ret <- f
    putContext context
    return ret

endStatement :: PreprocessorMonad ()
endStatement = do
    addWarning $ Debug "Ending statement"
    toDrop <- gets toDropAtStatementEnd
    traverse_ moveOutById toDrop
    clearVariablesToDrop
