module TypeCheck.StateFunctions where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Map (empty, lookup, insert, member)
import Control.Monad.State
import Control.Monad.Except

import Fe.Abs (Ident (Ident), BNFC'Position)

import Common.Scope
import Common.Utils
import Common.Types

import TypeCheck.State
import TypeCheck.Error
import Data.Foldable (traverse_)
import Data.Set (delete, singleton, insert)
import TypeCheck.Variable
import Data.List (isPrefixOf, intercalate)
import Common.Printer (CodePrint(codePrint))

makeNewFrame :: Variables -> Variables
makeNewFrame (Variables (Global global) variables) = Variables (Local (Global global) empty) variables
makeNewFrame (Variables (Local parent _) variables) = makeNewFrame (Variables parent variables)

getType :: Identifier -> PreprocessorMonad Type
getType identifier = do
    let (Identifier p ident) = identifier
    state <- get
    let maybeType = helper ident (typeDefinitions state)
    when (isNothing maybeType) $ do throwError $ TypeNotDefined identifier
    return $ fromJust maybeType
  where
    helper :: Ident -> TypeDefinitions -> Maybe Type
    helper ident (Global types) = lookup ident types
    helper ident (Local parent types) =
        let x = lookup ident types in
        if isNothing x then helper ident parent
        else x

getTypeLocal :: Identifier -> TypeDefinitions -> Maybe Type
getTypeLocal (Identifier _ ident) (Global types) = lookup ident types
getTypeLocal (Identifier _ ident) (Local _ types) = lookup ident types

addType :: Identifier -> Type -> PreprocessorMonad ()
addType identifier t = do
    let (Identifier _ ident) = identifier
    state <- get
    let maybeDefinition = getTypeLocal identifier (typeDefinitions state)
    when (isJust maybeDefinition) $ do throwError $ TypeAlreadyInScope identifier t (fromJust maybeDefinition)
    putTypeDefinitions (helper ident t (typeDefinitions state))
  where
    helper ident t (Global types) = Global (Data.Map.insert ident t types)
    helper ident t (Local parent types) = Local parent (Data.Map.insert ident t types)

getVariable :: Identifier -> PreprocessorMonad (VariableId, Variable)
getVariable identifier = do
    let Identifier _ ident = identifier
    Variables scope variables <- gets variables
    let maybeOut = helper ident scope variables
    when (isNothing maybeOut) $ throwError (VariableNotDefined identifier)
    return $ fromJust maybeOut
  where
    helper :: Ident -> VariableMappings -> [Variable] -> Maybe (VariableId, Variable)
    helper ident (Global mappings) variables = do
        id <- lookup ident mappings
        return (id, listGet id variables)
    helper ident (Local parent mappings) variables = do
        let id = lookup ident mappings
        if isNothing id then do
            helper ident parent variables
        else do
            let id' = fromJust id
            return (id', listGet id' variables)

getVariableById :: VariableId -> PreprocessorMonad Variable
getVariableById id = do
    Variables scope variables <- gets variables
    return $ listGet id variables

getLocalVariable :: Identifier -> PreprocessorMonad (Maybe (VariableId, Variable))
getLocalVariable (Identifier _ ident) = do
    variables <- gets variables
    return $ helper ident variables
  where
    helper ident (Variables (Global mappings) variables) = do
        id <- lookup ident mappings
        return (id, listGet id variables)
    helper ident (Variables (Local parent mappings) variables) = do
        id <- lookup ident mappings
        return (id, listGet id variables)

-- does not change lifetimes, takes it from the environment
addVariable :: Identifier -> Type -> VariableState -> PreprocessorMonad VariableId
addVariable identifier t variableState = do
    id <- checkShadowing identifier
    internalAddVariable identifier t variableState id

-- does not change lifetimes, takes it from the environment
addTemporaryVariable :: BNFC'Position -> Type -> PreprocessorMonad VariableId
addTemporaryVariable p t = do
    internalAddVariable (Identifier p (Ident "temporary")) t Free Nothing

internalAddVariable :: Identifier -> Type -> VariableState -> Maybe VariableId -> PreprocessorMonad VariableId
internalAddVariable identifier t variableState id = do
    variables <- gets variables
    lifetime <- getLifetime
    let variable = Variable {
        variableIdentifier = identifier,
        variableType = t,
        variableState = variableState,
        borrows = [],
        borrowsMut = [],
        lifetime = lifetime
    }
    helper identifier variable variables id
  where
    helper (Identifier _ ident) variable (Variables (Global mappings) variables) maybeId = do
        state <- get
        let id = fromMaybe
        let (id, variables') = if isJust maybeId then
                let x = fromJust maybeId in
                (x, listSet x variable variables)
            else
                (length variables, listPushBack variable variables)
        putVariables $ Variables (Global (Data.Map.insert ident id mappings)) variables'
        return id
    helper (Identifier _ ident) variable (Variables (Local parent mappings) variables) maybeId = do
        state <- get
        let id = fromMaybe (length variables) maybeId
        putVariables $ Variables (Local parent (Data.Map.insert ident id mappings)) (listPushBack variable variables)
        return id

inNewFrame :: BNFC'Position -> PreprocessorMonad a -> PreprocessorMonad a
inNewFrame p f = do
    state <- get
    lifetime <- forkLifetime p staticLifetime
    saveLifetime lifetime
    putVariables $ makeNewFrame (variables state)
    putTypeDefinitions $ Local (typeDefinitions state) Data.Map.empty
    ret' <- f
    reproduceWithPersistent state
    return ret'

inNewScope :: BNFC'Position -> PreprocessorMonad a -> PreprocessorMonad a
inNewScope p f = do
    state <- get -- record current state
    updateLifetime p
    let Variables mappings container = variables state
    putVariables $ Variables (Local mappings empty) container
    putTypeDefinitions $ Local (typeDefinitions state) Data.Map.empty
    ret' <- f
    reproduceWithPersistent state
    return ret'

reproduceWithPersistent :: PreprocessorState -> PreprocessorMonad ()
reproduceWithPersistent state = do
    warnings <- gets warnings
    LifetimeState _ id <- gets lifetimeState
    put state

    LifetimeState lifetime _ <- gets lifetimeState
    putWarnings warnings
    putLifetimeState (LifetimeState lifetime id)

checkShadowing :: Identifier -> PreprocessorMonad (Maybe VariableId)
checkShadowing identifier = do
    maybeVar <- getLocalVariable identifier
    unless (isNothing maybeVar || shadowingOk (fromJust maybeVar)) $ do
        let Just (_, originalVariable) = maybeVar
        addWarning (Shadow (variableIdentifier originalVariable) identifier)
    return $ do
        (i, _) <- maybeVar
        Just i
  where
    shadowingOk (_, Variable _ t variableState _ _ _) =
        isFunction t && variableState == Uninitialized

markVariableUsed :: VariableId -> PreprocessorMonad ()
markVariableUsed id = do
    PreprocessorState typeDefinitions variables currentLifetime warnings usedVariables <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings (listPushBack id usedVariables)

handleUsedVariables :: (VariableId -> PreprocessorMonad ()) -> PreprocessorMonad ()
handleUsedVariables handler = do
    usedVariables <- gets usedVariables
    traverse_ handler usedVariables
    clearUsedVariables

moveOutVariable :: VariableId -> PreprocessorMonad ()
moveOutVariable id = do
    Variables _ variables <- gets variables
    let Variable _ _ _ borrows borrowsMut _ = listGet id variables
    addWarning $ Debug ("Moving out " ++ show id)
    traverse_ (removeBorrow id) borrows
    traverse_ removeMutBorrow borrowsMut
  where
    removeBorrow :: VariableId -> VariableId -> PreprocessorMonad ()
    removeBorrow borrowerId borrowedId = do
        addWarning $ Debug ("    Removing borrow of " ++ show borrowedId)
        Variables mappings variables <- gets variables
        let Variable ident t (Borrowed whatBorrowed) borrows borrowsMut lifetime = listGet borrowedId variables
        let state' = if null whatBorrowed then Free
                else Borrowed (delete borrowerId whatBorrowed)
        let variable' = Variable ident t state' borrows borrowsMut lifetime
        let variables' = listSet borrowedId variable' variables
        putVariables $ Variables mappings variables'
        return ()
    removeMutBorrow :: VariableId -> PreprocessorMonad ()
    removeMutBorrow borrowedId = do
        addWarning $ Debug ("    Removing mutable borrow of " ++ show borrowedId)
        Variables mappings variables <- gets variables
        let Variable ident t (BorrowedMut _) borrows borrowsMut lifetime = listGet borrowedId variables
        let variable' = Variable ident t Free borrows borrowsMut lifetime
        let variables' = listSet borrowedId variable' variables
        putVariables $ Variables mappings variables'
        return ()

borrowVariable :: VariableId -> VariableId -> PreprocessorMonad ()
borrowVariable borrowerId borrowedId = borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed
  where
    markAsBorrowed :: Variable -> PreprocessorMonad Variable
    markAsBorrowed (Variable ident t state borrows borrowsMut lifetime) = do
        state' <- if state == Free then do
                return $ Borrowed (singleton borrowerId)
        else if isBorrowed state then do
                let Borrowed whatBorrows = state
                return $ Borrowed (Data.Set.insert borrowerId whatBorrows)
        else do
            throwError (CannotBorrow borrowerId ident)
        return $ Variable ident t state' borrows borrowsMut lifetime
    addBorrowed :: Variable -> PreprocessorMonad Variable
    addBorrowed (Variable ident t state borrows borrowsMut lifetime) = do
        return $ Variable ident t state (listPushBack borrowedId borrows) borrowsMut lifetime

borrowMutVariable :: VariableId -> VariableId -> PreprocessorMonad ()
borrowMutVariable borrowerId borrowedId = borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed
  where
    markAsBorrowed :: Variable -> PreprocessorMonad Variable
    markAsBorrowed (Variable ident t state borrows borrowsMut lifetime) = do
        unless (state == Free) $ throwError (CannotBorrow borrowerId ident)
        return $ Variable ident t (BorrowedMut borrowerId) borrows borrowsMut lifetime
    addBorrowed :: Variable -> PreprocessorMonad Variable
    addBorrowed (Variable ident t state borrows borrowsMut lifetime) = do
        return $ Variable ident t state borrows (listPushBack borrowedId borrowsMut) lifetime

borrowInternal :: VariableId -> VariableId -> (Variable -> PreprocessorMonad Variable) -> (Variable -> PreprocessorMonad Variable) -> PreprocessorMonad ()
borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed = do
    let warning1 = show borrowerId ++ " borrows " ++ show borrowedId
    Variables mappings variables <- gets variables
    let warning2 = "\n    state before: [\n" ++ intercalate ",\n" (fmap (codePrint 2) variables) ++ "]"
    borrowerVariable' <- addBorrowed (listGet borrowerId variables)
    borrowedVariable' <- markAsBorrowed (listGet borrowedId variables)
    let variables' = listSet borrowerId borrowerVariable' variables
    let variables'' = listSet borrowedId borrowedVariable' variables'
    let warning3 = "\n    state after: [\n" ++ intercalate ",\n" (fmap (codePrint 2) variables'') ++ "]"
    addWarning $ Debug (warning1 ++ warning2 ++ warning3)
    putVariables $ Variables mappings variables''

-- TODO: Dobrze by to było przetestować
-- Teoretycznie jeśli zrobi się
-- borrowVariable 1 2 (1 borrows 2)
-- moveOut 1
-- to 2 powinna być bez borrow

-- first is subLifetime of second if and only if first lives longer or equal compared to second. (therefore second lifetime can be used in place of the first one)
isSubLifetime :: Lifetime -> Lifetime -> Bool
isSubLifetime (Lifetime first _) (Lifetime second _) = first `isPrefixOf` second

-- updates lifetime state, but keeps current lifetime saved value
forkLifetime :: BNFC'Position -> Lifetime -> PreprocessorMonad Lifetime
forkLifetime p (Lifetime list _) = do
    let line = if isJust p then
            let (l, _) = fromJust p in l
        else
            -1
    LifetimeState savedLifetime id <- gets lifetimeState
    let lifetime' = Lifetime (listPushBack id list) line
    putLifetimeState $ LifetimeState savedLifetime (id + 1)
    return lifetime'

-- updates lifetime state and changes saved lifetime
updateLifetime :: BNFC'Position -> PreprocessorMonad ()
updateLifetime p = do
    lifetime <- getLifetime
    lifetime' <- forkLifetime p lifetime
    saveLifetime lifetime'

getLifetime :: PreprocessorMonad Lifetime
getLifetime = do
    LifetimeState lifetime _ <- gets lifetimeState
    return lifetime

saveLifetime :: Lifetime -> PreprocessorMonad ()
saveLifetime lifetime = do
    LifetimeState _ id <- gets lifetimeState
    putLifetimeState $ LifetimeState lifetime id
