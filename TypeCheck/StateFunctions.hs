module TypeCheck.StateFunctions where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Map (empty, lookup, insert, member, elems)
import Data.Set (delete, singleton, insert)
import Data.List (isPrefixOf, intercalate, nub)
import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.Except

import Fe.Abs (Ident (Ident), BNFC'Position, HasPosition (hasPosition))

import Common.Scope
import Common.Utils
import Common.Types
import Common.Printer

import TypeCheck.State
import TypeCheck.Error
import TypeCheck.Variable
import Common.Ast
import qualified Fe.Abs as A

makeNewFrame :: Variables -> Variables
makeNewFrame (Variables (Global global) variables) = Variables (Local (Global global) empty) variables
makeNewFrame (Variables (Local parent _) variables) = makeNewFrame (Variables parent variables)

getType :: Identifier -> PreprocessorMonad Type
getType identifier = do
    let (Identifier p ident) = identifier
    state <- get
    let maybeType = helper ident (typeDefinitions state)
    when (isNothing maybeType) $ do throw $ TypeNotDefined identifier
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
    when (isJust maybeDefinition) $ do throw $ TypeAlreadyInScope identifier t (fromJust maybeDefinition)
    putTypeDefinitions (helper ident t (typeDefinitions state))
  where
    helper ident t (Global types) = Global (Data.Map.insert ident t types)
    helper ident t (Local parent types) = Local parent (Data.Map.insert ident t types)

getVariable :: Identifier -> PreprocessorMonad (VariableId, Variable)
getVariable identifier = do
    let Identifier _ ident = identifier
    Variables scope variables <- gets variables
    let maybeOut = helper ident scope variables
    when (isNothing maybeOut) $ throw (VariableNotDefined identifier)
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
addVariable :: Identifier -> Bool -> Type -> VariableState -> PreprocessorMonad VariableId
addVariable identifier isConst t variableState = do
    id <- checkShadowing identifier
    internalAddVariable identifier isConst t variableState id

-- does not change lifetimes, takes it from the environment
addTemporaryVariable :: BNFC'Position -> Type -> PreprocessorMonad VariableId
addTemporaryVariable p t = do
    internalAddVariable (Identifier p (Ident "temporary")) True t Free Nothing

internalAddVariable :: Identifier -> Bool -> Type -> VariableState -> Maybe VariableId -> PreprocessorMonad VariableId
internalAddVariable identifier isConst t variableState id = do
    variables <- gets variables
    lifetime <- getLifetime
    let variable = Variable {
        variableIdentifier = identifier,
        variableIsConst = isConst,
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

setVariableById :: VariableId -> Variable -> PreprocessorMonad ()
setVariableById id variable = do
    Variables mappings variables <- gets variables
    let variables' = listSet id variable variables
    putVariables $ Variables mappings variables'

mutateVariableById :: VariableId -> (Variable -> Variable) -> PreprocessorMonad ()
mutateVariableById id mutate = do
    variable <- getVariableById id
    setVariableById id (mutate variable)

inNewFrame :: BNFC'Position -> PreprocessorMonad a -> PreprocessorMonad a
inNewFrame p f = do
    state <- get
    lifetime <- forkLifetime p staticLifetime
    saveLifetime lifetime
    putVariables $ makeNewFrame (variables state)
    putTypeDefinitions $ Local (typeDefinitions state) Data.Map.empty
    ret' <- f
    clearUsedVariables
    reproduceWithPersistent p state
    return ret'

inNewScope :: BNFC'Position -> PreprocessorMonad a -> PreprocessorMonad a
inNewScope p f = do
    state <- get -- record current state
    updateLifetime p
    let Variables mappings container = variables state
    putVariables $ Variables (Local mappings empty) container
    putTypeDefinitions $ Local (typeDefinitions state) Data.Map.empty
    ret <- f
    reproduceWithPersistent p state
    return ret

reproduceWithPersistent :: BNFC'Position -> PreprocessorState -> PreprocessorMonad ()
reproduceWithPersistent p state = do
    usedVariables <- gets usedVariables
    maybeVar <- if isJust usedVariables then do
            let usedId = fromJust usedVariables
            var <- getVariableById usedId 
            return $ Just var
        else do
            return Nothing

    variables <- gets variables
    let Variables (Local _ map) _ = variables
    traverse_ moveOutVariable (reverse (elems map))

    warnings <- gets warnings
    LifetimeState _ id <- gets lifetimeState

    put state

    when (isJust maybeVar) $ do
        let template = fromJust maybeVar
        tempVar <- addTemporaryVariable p (variableType template)
        traverse_ (borrowVariable p tempVar) (borrows template)
        traverse_ (borrowMutVariable p tempVar) (borrowsMut template)
        markVariableUsed tempVar

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
    shadowingOk (_, Variable _ _ t variableState _ _ _) =
        isFunction t && variableState == Uninitialized

markVariableUsed :: VariableId -> PreprocessorMonad ()
markVariableUsed id = do
    PreprocessorState typeDefinitions variables currentLifetime warnings usedVariables <- get
    when (isJust usedVariables) $ do
        printVariables
        throw (Fatal ("Marking variable " ++ show id ++ " as used, when another variable is used " ++ show (fromJust usedVariables)))
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings (Just id)

handleUsedVariables :: (VariableId -> PreprocessorMonad ()) -> PreprocessorMonad ()
handleUsedVariables handler = do
    usedVariables <- gets usedVariables
    traverse_ handler usedVariables
    clearUsedVariables

moveOutVariable :: VariableId -> PreprocessorMonad ()
moveOutVariable id = do
    variable <- getVariableById id

    shouldMove <- internalShouldMove variable
    if not shouldMove then do return ()
    else do

    addWarning $ Debug ("Moving out " ++ show id ++ " " ++ show (variableIdentifier variable))
    traverse_ (removeBorrow id) (borrows variable)
    traverse_ removeMutBorrow (borrowsMut variable)
    setVariableById id (setVariableState Moved variable)
  where
    removeBorrow :: VariableId -> VariableId -> PreprocessorMonad ()
    removeBorrow borrowerId borrowedId = do
        addWarning $ Debug ("    Removing borrow of " ++ show borrowedId)
        variable <- getVariableById borrowedId
        let Borrowed whatBorrowed = variableState variable
        let whatBorrowed' = delete borrowerId whatBorrowed
        let state' = if null whatBorrowed' then Free
                else Borrowed whatBorrowed'
        setVariableById borrowedId (setVariableState state' variable)
        return ()
    removeMutBorrow :: VariableId -> PreprocessorMonad ()
    removeMutBorrow borrowedId = do
        addWarning $ Debug ("    Removing mutable borrow of " ++ show borrowedId)
        mutateVariableById borrowedId (setVariableState Free)
        return ()

transferOwnership  :: VariableId -> VariableId -> PreprocessorMonad ()
transferOwnership newOwnerId movedOutId = do
    newOwner <- getVariableById newOwnerId
    movedOut <- getVariableById movedOutId

    shouldMove <- internalShouldMove movedOut
    if not shouldMove then do return ()
    else do

    addWarning $ Debug ("Transfering " ++ show movedOutId ++ " to " ++ show newOwnerId)

    newOwnerBorrows <- foldM (transferBorrow newOwnerId movedOutId) (borrows newOwner) (borrows movedOut)
    newOwnerBorrowsMut <- foldM (transferMutBorrow newOwnerId) (borrowsMut newOwner) (borrowsMut movedOut)

    setVariableById newOwnerId (changeVariable (variableState newOwner) (nub newOwnerBorrows) (nub newOwnerBorrowsMut) newOwner)
    setVariableById movedOutId (changeVariable Moved [] [] movedOut)
  where
    transferBorrow :: VariableId -> VariableId -> [VariableId] -> VariableId -> PreprocessorMonad [VariableId]
    transferBorrow newOwnerId movedOutId combinedBorrowsList borrowedId = do
        variable <- getVariableById borrowedId
        let Borrowed whatBorrowed = variableState variable
        let whatBorrowed' = delete movedOutId whatBorrowed
        let whatBorrowed'' = Data.Set.insert newOwnerId whatBorrowed'
        setVariableById borrowedId (setVariableState (Borrowed whatBorrowed'') variable)
        return $ borrowedId:combinedBorrowsList
    transferMutBorrow :: VariableId -> [VariableId] -> VariableId -> PreprocessorMonad [VariableId]
    transferMutBorrow newOwnerId combinedBorrowsList borrowedId = do
        mutateVariableById borrowedId (setVariableState (BorrowedMut newOwnerId))
        return $ borrowedId:combinedBorrowsList

internalShouldMove :: Variable -> PreprocessorMonad Bool
internalShouldMove variable = do
    let state = variableState variable
    if state == Moved then do
        return False
    else if state == Uninitialized then do
        addWarning $ VariableNotInitializedNotUsed (variableIdentifier variable)
        return False
    else do
        unless (state == Free) $ throw (CannotMoveOut variable)
        if isOnceFunction (variableType variable) then do
            return False
        else do
            return True

borrowVariable :: A.BNFC'Position -> VariableId -> VariableId -> PreprocessorMonad ()
borrowVariable p borrowerId borrowedId = borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed
  where
    markAsBorrowed :: Variable -> PreprocessorMonad Variable
    markAsBorrowed (Variable ident const t state borrows borrowsMut lifetime) = do
        state' <- if state == Free then do
                return $ Borrowed (singleton borrowerId)
        else if isBorrowed state then do
                let Borrowed whatBorrows = state
                return $ Borrowed (Data.Set.insert borrowerId whatBorrows)
        else do
            throw (AlreadyBorrowed borrowerId p)
        return $ Variable ident const t state' borrows borrowsMut lifetime
    addBorrowed :: Variable -> PreprocessorMonad Variable
    addBorrowed (Variable ident const t state borrows borrowsMut lifetime) = do
        return $ Variable ident const t state (listPushBack borrowedId borrows) borrowsMut lifetime

borrowMutVariable :: A.BNFC'Position -> VariableId -> VariableId -> PreprocessorMonad ()
borrowMutVariable p borrowerId borrowedId = borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed
  where
    markAsBorrowed :: Variable -> PreprocessorMonad Variable
    markAsBorrowed (Variable ident const t state borrows borrowsMut lifetime) = do
        unless (state == Free) $ throw (AlreadyBorrowed borrowerId p)
        return $ Variable ident const t (BorrowedMut borrowerId) borrows borrowsMut lifetime
    addBorrowed :: Variable -> PreprocessorMonad Variable
    addBorrowed (Variable ident const t state borrows borrowsMut lifetime) = do
        return $ Variable ident const t state borrows (listPushBack borrowedId borrowsMut) lifetime

borrowInternal :: VariableId -> VariableId -> (Variable -> PreprocessorMonad Variable) -> (Variable -> PreprocessorMonad Variable) -> PreprocessorMonad ()
borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed = do
    getVariableById borrowerId >>= addBorrowed >>= setVariableById borrowerId
    getVariableById borrowedId >>= markAsBorrowed >>= setVariableById borrowedId

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


printVariables :: PreprocessorMonad ()
printVariables = do
    Variables _ variables <- gets variables
    addWarning $ Debug ("Variables: [\n" ++ intercalate ",\n" (fmap (codePrint 2) variables) ++ "]")

printUsedVariables :: String -> PreprocessorMonad ()
printUsedVariables text = do 
    used <- gets usedVariables
    addWarning $ Debug (text ++ show used)