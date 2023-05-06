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

-- borrowVariable :: A.BNFC'Position -> VariableId -> VariableId -> PreprocessorMonad ()
-- borrowVariable p borrowerId borrowedId = borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed
--   where
--     markAsBorrowed :: Variable -> PreprocessorMonad Variable
--     markAsBorrowed (Variable ident const t state borrows borrowsMut lifetime) = do
--         state' <- if state == Free then do
--             return $ Borrowed (singleton borrowerId)
--         else if state == Uninitialized then do
--             throw $ UninitializedVariableUsed p ident
--         else if isBorrowed state then do
--             let Borrowed whatBorrows = state
--             return $ Borrowed (Data.Set.insert borrowerId whatBorrows)
--         else do
--             throw $ AlreadyBorrowed borrowerId p
--         return $ Variable ident const t state' borrows borrowsMut lifetime
--     addBorrowed :: Variable -> PreprocessorMonad Variable
--     addBorrowed (Variable ident const t state borrows borrowsMut lifetime) = do
--         return $ Variable ident const t state (listPushBack borrowedId borrows) borrowsMut lifetime

-- borrowMutVariable :: A.BNFC'Position -> VariableId -> VariableId -> PreprocessorMonad ()
-- borrowMutVariable p borrowerId borrowedId = borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed
--   where
--     markAsBorrowed :: Variable -> PreprocessorMonad Variable
--     markAsBorrowed (Variable ident mutability t state borrows borrowsMut lifetime) = do
--         unless (state == Free || state == Uninitialized) $ throw (AlreadyBorrowed borrowedId p)
--         when (isConst mutability) $ throw (CannotTakeMutableReferenceToConstant p borrowedId)
--         return $ Variable ident mutability t (BorrowedMut borrowerId) borrows borrowsMut lifetime
--     addBorrowed :: Variable -> PreprocessorMonad Variable
--     addBorrowed (Variable ident const t state borrows borrowsMut lifetime) = do
--         return $ Variable ident const t state borrows (listPushBack borrowedId borrowsMut) lifetime

-- borrowInternal :: VariableId -> VariableId -> (Variable -> PreprocessorMonad Variable) -> (Variable -> PreprocessorMonad Variable) -> PreprocessorMonad ()
-- borrowInternal borrowerId borrowedId markAsBorrowed addBorrowed = do
--     getVariableById borrowerId >>= addBorrowed >>= setVariableById borrowerId
--     getVariableById borrowedId >>= markAsBorrowed >>= setVariableById borrowedId


tryMoveOutById :: VariableId -> PreprocessorMonad ()
tryMoveOutById variableId = do
    variable <- getVariableById variableId
    tryMoveOut variableId variable

tryMoveOut :: VariableId -> Variable -> PreprocessorMonad ()
tryMoveOut id variable = do
    canMove <- canMove variable
    if not canMove then do return ()
    else do
    
    addWarning $ Debug ("Moving out " ++ show id ++ " " ++ show (variableName variable))
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