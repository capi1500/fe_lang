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
import TypeCheck.Printer

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
            throw $ UninitializedVariableUsed p (variableId variable)
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
    if isCopy (variableType variable) then do
        return ()
    else do
        moveOut variable

moveOutById :: VariableId -> PreprocessorMonad ()
moveOutById variableId = do
    variable <- getVariableById variableId
    moveOut variable

moveOut :: Variable -> PreprocessorMonad ()
moveOut variable = do
    p <- gets position
    printDebug ("Moving out " ++ codePrint 1 variable ++ " at " ++ show p)
    let state = variableState variable
    if state == Moved then do
        return ()
    else if state == Uninitialized then do
        addWarning $ VariableNotInitializedNotUsed (variableId variable)
    else  do
        unless (state == Free) $ throw (CannotMoveOut variable)
        let value = variableValue variable
        when (owned value) $ dropValue value
        setVariableById (variableId variable) (setVariableState Moved variable)

makeBorrow :: VariableId -> Mutable -> PreprocessorMonad Value
makeBorrow id mut = do
    p <- gets position
    var <- getVariableById id
    doBorrow p mut (variableType var)
  where
    doBorrow p Const t = do
        borrow id
        return Value {
            valueCreatedAt = p,
            valueType = TReference Const t,
            ownedPlaces = [],
            borrows = [id],
            borrowsMut = [],
            owned = True
        }
    doBorrow p Mutable t = do
        borrowMut id
        return Value {
            valueCreatedAt = p,
            valueType = TReference Mutable t,
            ownedPlaces = [],
            borrows = [],
            borrowsMut = [id],
            owned = True
        }


makeImplicitBorrowValue :: VariableId -> Mutable -> PreprocessorMonad Value
makeImplicitBorrowValue id mutability = do
    value <- makeBorrow id mutability
    printDebug ("implicit borrow of " ++ show id)
    return (setValueOwned False value)

dropValue :: Value -> PreprocessorMonad ()
dropValue value = do
    printDebug ("Dropping " ++ codePrint 1 value)
    traverse_ moveOutById (ownedPlaces value)
    traverse_ removeBorrow (borrows value)
    traverse_ removeBorrowMut (borrowsMut value)
  where
    removeBorrow :: VariableId -> PreprocessorMonad ()
    removeBorrow borrowedId = do
        printDebug ("    Removing borrow of " ++ show borrowedId ++ " (at " ++ show (valueCreatedAt value) ++ ")")
        variable <- getVariableById borrowedId
        let Borrowed borrowersCount borrowPositions = variableState variable
        let state' = if borrowersCount == 1 then Free
                else Borrowed (borrowersCount - 1) (delete (valueCreatedAt value) borrowPositions)
        printDebug ("   New variable state " ++ show state')
        setVariableById borrowedId (setVariableState state' variable)
        return ()
    removeBorrowMut :: VariableId -> PreprocessorMonad ()
    removeBorrowMut borrowedId = do
        printDebug ("    Removing mutable borrow of " ++ show borrowedId)
        mutateVariableById borrowedId (setVariableState Free)
        return ()
