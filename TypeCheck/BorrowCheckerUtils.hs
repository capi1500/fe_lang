module TypeCheck.BorrowCheckerUtils where

import Prelude hiding (null)
import Data.Set
import Data.Foldable hiding (null)
import Control.Monad.State

import Common.Types

import TypeCheck.Variable
import TypeCheck.State
import TypeCheck.Error
import TypeCheck.VariablesUtils
import Text.XHtml (variable)
import Data.Maybe
import Common.Printer

-- transferOwnership  :: VariableId -> VariableId -> PreprocessorMonad ()
-- transferOwnership newOwnerId movedOutId = do
--     newOwner <- getVariableById newOwnerId
--     movedOut <- getVariableById movedOutId

--     shouldMove <- internalShouldMove movedOut
--     if not shouldMove then do return ()
--     else do

--     addWarning $ Debug ("Transfering " ++ show movedOutId ++ " to " ++ show newOwnerId)

--     newOwnerBorrows <- foldM (transferBorrow newOwnerId movedOutId) (borrows newOwner) (borrows movedOut)
--     newOwnerBorrowsMut <- foldM (transferMutBorrow newOwnerId) (borrowsMut newOwner) (borrowsMut movedOut)

--     setVariableById newOwnerId (changeVariable (variableState newOwner) (nub newOwnerBorrows) (nub newOwnerBorrowsMut) newOwner)
--     setVariableById movedOutId (changeVariable Moved [] [] movedOut)
--   where
--     transferBorrow :: VariableId -> VariableId -> [VariableId] -> VariableId -> PreprocessorMonad [VariableId]
--     transferBorrow newOwnerId movedOutId combinedBorrowsList borrowedId = do
--         variable <- getVariableById borrowedId
--         let Borrowed whatBorrowed = variableState variable
--         let whatBorrowed' = delete movedOutId whatBorrowed
--         let whatBorrowed'' = Data.Set.insert newOwnerId whatBorrowed'
--         setVariableById borrowedId (setVariableState (Borrowed whatBorrowed'') variable)
--         return $ borrowedId:combinedBorrowsList
--     transferMutBorrow :: VariableId -> [VariableId] -> VariableId -> PreprocessorMonad [VariableId]
--     transferMutBorrow newOwnerId combinedBorrowsList borrowedId = do
--         mutateVariableById borrowedId (setVariableState (BorrowedMut newOwnerId))
--         return $ borrowedId:combinedBorrowsList

borrow :: VariableId -> PreprocessorMonad ()
borrow borrowedId = borrowInternal borrowedId markAsBorrowed
  where
    markAsBorrowed :: Variable -> PreprocessorMonad Variable
    markAsBorrowed variable = do
        p <- gets position
        let state = variableState variable
        state' <- if state == Free then do
            return $ Borrowed 1 (singleton p)
        else if state == Uninitialized then do
            throw $ UninitializedVariableUsed p (fromJust (variableName variable))
        else if isBorrowed state then do
            let Borrowed borrowedCount borrowPositions = state
            return $ Borrowed (borrowedCount + 1) (insert p borrowPositions)
        else do
            throw $ AlreadyBorrowed p borrowedId
        return $ setVariableState state' variable

borrowMut :: VariableId -> PreprocessorMonad ()
borrowMut borrowedId = borrowInternal borrowedId markAsBorrowed
  where
    markAsBorrowed :: Variable -> PreprocessorMonad Variable
    markAsBorrowed variable = do
        p <- gets position
        let state = variableState variable
        unless (state == Free || state == Uninitialized) $ throw (AlreadyBorrowed p borrowedId)
        when (isConst (variableMutability variable)) $ throw (CannotTakeMutableReferenceToConstant p borrowedId)
        return $ setVariableState (BorrowedMut p) variable

borrowInternal :: VariableId -> (Variable -> PreprocessorMonad Variable) -> PreprocessorMonad ()
borrowInternal borrowedId markAsBorrowed = do
    getVariableById borrowedId >>= markAsBorrowed >>= setVariableById borrowedId


moveOutOrCopyById :: VariableId -> PreprocessorMonad ()
moveOutOrCopyById variableId = do
    variable <- getVariableById variableId
    moveOutOrCopy variable

moveOutOrCopy :: Variable -> PreprocessorMonad ()
moveOutOrCopy variable = do
    canMove <- canMove variable
    if not canMove then do return ()
    else do moveOut variable

moveOutById :: VariableId -> PreprocessorMonad ()
moveOutById variableId = do
    variable <- getVariableById variableId
    moveOut variable

moveOut :: Variable -> PreprocessorMonad ()
moveOut variable = do
    addWarning $ Debug ("Moving out " ++ show (variableId variable) ++ " " ++ show (variableName variable))
    let value = variableValue variable
    when (owned value) $ dropValue value
    setVariableById (variableId variable) (setVariableState Moved variable) 

canMoveById :: VariableId -> PreprocessorMonad Bool
canMoveById variableId = do
    variable <- getVariableById variableId
    canMove variable

canMove :: Variable -> PreprocessorMonad Bool
canMove variable = do
    let state = variableState variable
    if state == Moved then do
        return False
    else if state == Uninitialized then do
        addWarning $ VariableNotInitializedNotUsed (variableId variable)
        return False
    else do
        unless (state == Free) $ throw (CannotMoveOut variable)
        let t = variableType variable
        return $ not (isCopy t)

makeImplicitBorrowValue :: VariableId -> Mutable -> PreprocessorMonad (Value, VariableId)
makeImplicitBorrowValue id mutability = do
    p <- gets position
    variable <- getVariableById id
    value <- makeValue mutability (variableType variable) p
    tempId <- addTemporaryVariable mutability value
    addWarning $ Debug ("implicit borrow of " ++ show id ++ " as " ++ show tempId)
    return (value, tempId)
  where
    makeValue Const t p = do
        let value = Value {
            valueCreatedAt = p,
            valueType = TReference Const t,
            borrows = [id],
            borrowsMut = [],
            owned = True
        }
        borrow id
        return value
    makeValue Mutable t p = do
        let value = Value {
            valueCreatedAt = p,
            valueType = TReference Mutable t,
            borrows = [],
            borrowsMut = [id],
            owned = True
        }
        borrowMut id
        return value

dropValue :: Value -> PreprocessorMonad ()
dropValue value = do
    addWarning $ Debug ("Dropping " ++ codePrint 1 value)
    traverse_ removeBorrow (borrows value)
    traverse_ removeBorrowMut (borrowsMut value)
  where
    removeBorrow :: VariableId -> PreprocessorMonad ()
    removeBorrow borrowedId = do
        addWarning $ Debug ("    Removing borrow of " ++ show borrowedId ++ " (at " ++ show (valueCreatedAt value) ++ ")")
        variable <- getVariableById borrowedId
        let Borrowed borrowersCount borrowPositions = variableState variable
        let state' = if borrowersCount == 1 then Free
                else Borrowed (borrowersCount - 1) (delete (valueCreatedAt value) borrowPositions)
        addWarning $ Debug ("   New variable state " ++ show state')
        setVariableById borrowedId (setVariableState state' variable)
        return ()
    removeBorrowMut :: VariableId -> PreprocessorMonad ()
    removeBorrowMut borrowedId = do
        addWarning $ Debug ("    Removing mutable borrow of " ++ show borrowedId)
        mutateVariableById borrowedId (setVariableState Free)
        return ()
