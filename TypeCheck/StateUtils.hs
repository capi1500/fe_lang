module TypeCheck.StateUtils where

import Data.List
import Data.Foldable
import Data.Map
import Control.Monad.State

import Fe.Abs (BNFC'Position)

import Common.Scope
import Common.Printer

import TypeCheck.LifetimeUtils
import TypeCheck.VariablesUtils
import TypeCheck.ExpressionEval
import TypeCheck.Error
import TypeCheck.State
import TypeCheck.BorrowCheckerUtils (tryMoveOutById)




makeNewFrame :: Variables -> Variables
makeNewFrame (Variables (Global global) variables) = Variables (Local (Global global) empty) variables
makeNewFrame (Variables (Local parent _) variables) = makeNewFrame (Variables parent variables)

inNewFrame :: BNFC'Position -> PreprocessorMonad a -> PreprocessorMonad a
inNewFrame p f = do
    state <- get
    lifetime <- forkLifetime p staticLifetime
    saveLifetime lifetime
    putVariables $ makeNewFrame (variables state)
    putTypeDefinitions $ Local (typeDefinitions state) empty
    ret' <- f
    reproduceWithPersistent p state
    return ret'

inNewScope :: BNFC'Position -> PreprocessorMonad a -> PreprocessorMonad a
inNewScope p f = do
    state <- get -- record current state
    updateLifetime p
    let Variables mappings container = variables state
    putVariables $ Variables (Local mappings empty) container
    putTypeDefinitions $ Local (typeDefinitions state) empty
    ret <- f
    reproduceWithPersistent p state
    return ret

reproduceWithPersistent :: BNFC'Position -> PreprocessorState -> PreprocessorMonad ()
reproduceWithPersistent p state = do
    -- maybeVar <- if isJust usedVariables then do
    --         let usedId = fromJust usedVariables
    --         var <- getVariableById usedId
    --         return $ Just var
    --     else do
    --         return Nothing

    variables <- gets variables
    let Variables (Local _ map) _ = variables
    traverse_ tryMoveOutById (reverse (elems map))

    warnings <- gets warnings
    LifetimeState _ id <- gets lifetimeState

    put state

    -- when (isJust maybeVar) $ do
    --     let template = fromJust maybeVar
    --     tempVar <- addTemporaryVariable p (variableMutability template) (variableType template)
    --     traverse_ (borrowVariable p tempVar) (borrows template)
    --     traverse_ (borrowMutVariable p tempVar) (borrowsMut template)
    --     markVariableUsed tempVar

    LifetimeState lifetime _ <- gets lifetimeState
    putWarnings warnings
    putLifetimeState (LifetimeState lifetime id)

whenContext :: ExpressionContext -> PreprocessorMonad () -> PreprocessorMonad ()
whenContext expected f = do
    context <- gets context
    when (context == expected) f

unlessContext :: ExpressionContext -> PreprocessorMonad () -> PreprocessorMonad ()
unlessContext expected f = do
    context <- gets context
    unless (context == expected) f

printVariables :: PreprocessorMonad ()
printVariables = do
    Variables _ variables <- gets variables
    addWarning $ Debug ("Variables: [\n" ++
        intercalate ",\n"
            (fmap
                (\(v, i) -> show i ++ ": " ++ codePrint 2 v)
                (zip variables [0 .. (length variables)]))
        ++ "]")

endStatement :: PreprocessorMonad ()
endStatement = do
    toDrop <- gets toDropAtStatementEnd
    traverse_ tryMoveOutById toDrop
    clearVariablesToDrop
