module TypeCheck.BorrowCheckerUtils where

import Prelude hiding (null)
import Data.Set
import Data.Maybe
import Data.Foldable hiding (null)
import Control.Monad.State

import Fe.Abs (BNFC'Position)

import Common.Types
import Common.Printer

import TypeCheck.Variable
import TypeCheck.State
import TypeCheck.Error
import TypeCheck.VariablesUtils
import TypeCheck.Printer

borrow :: (VariableId, BNFC'Position) -> PreprocessorMonad ()
borrow borrow' = borrowInternal borrow' markAsBorrowed
  where
    borrowedId = fst borrow'

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

borrowMut :: (VariableId, BNFC'Position) -> PreprocessorMonad ()
borrowMut borrow' = borrowInternal borrow' markAsBorrowed
  where
    borrowedId = fst borrow'

    markAsBorrowed :: Variable -> PreprocessorMonad Variable
    markAsBorrowed variable = do
        p <- gets position
        let state = variableState variable
        unless (state == Free || state == Uninitialized) $ throw (AlreadyBorrowed p borrowedId)
        when (isConst (variableMutability variable)) $ throw (CannotTakeMutableReferenceToConstant p borrowedId)
        return $ setVariableState (BorrowedMut p) variable

borrowInternal :: (VariableId, BNFC'Position) -> (Variable -> PreprocessorMonad Variable) -> PreprocessorMonad ()
borrowInternal borrow markAsBorrowed = do
    let borrowedId = fst borrow
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
    let state = variableState variable
    if state == Moved then do
        return ()
    else if state == Uninitialized then do
        addWarning $ VariableNotInitializedNotUsed (variableId variable)
    else  do
        unless (state == Free) $ throw (CannotMoveOut p variable)
        let value = variableValue variable
        when (owned value == ByVariable) $ dropValue value
        setVariableById (variableId variable) (setVariableState Moved variable)

addBorrow :: Value -> VariableId -> Mutable -> PreprocessorMonad Value
addBorrow value id mut = do
    p <- gets position
    var <- getVariableById id
    let b = (id, p)
    if mut == Mutable then do
        borrowMut b
        return $ addMutBorrowToValue b value
    else do
        borrow b
        return $ addBorrowToValue b value


makeBorrow :: VariableId -> Mutable -> PreprocessorMonad Value
makeBorrow id mut = do
    p <- gets position
    var <- getVariableById id
    doBorrow p mut (variableType var)
  where
    doBorrow p Const t = do
        borrow (id, p)
        return Value {
            valueType = TReference Const t,
            ownedPlaces = [],
            borrows = [(id, p)],
            borrowsMut = [],
            owned = ByVariable
        }
    doBorrow p Mutable t = do
        borrowMut (id, p)
        return Value {
            valueType = TReference Mutable t,
            ownedPlaces = [],
            borrows = [],
            borrowsMut = [(id, p)],
            owned = ByVariable
        }


makeImplicitBorrowValue :: VariableId -> Mutable -> PreprocessorMonad Value
makeImplicitBorrowValue id mutability = do
    value <- makeBorrow id mutability
    return (setValueOwned ByExpression value)

dropValue :: Value -> PreprocessorMonad ()
dropValue value = do
    if isOwnershipByBorrow (owned value) then do
        let ByBorrow id = owned value
        var <- getVariableById id
        let var' = setVariableState Free var
        let var'' = setVariableValue (setValueOwned ByVariable value) var'
        setVariableById id var''
    else do
        traverse_ moveOutById (ownedPlaces value)
        traverse_ removeBorrow (borrows value)
        traverse_ removeBorrowMut (borrowsMut value)
  where
    removeBorrow :: (VariableId, BNFC'Position) -> PreprocessorMonad ()
    removeBorrow borrow = do
        let borrowedId = fst borrow
        let p = snd borrow
        variable <- getVariableById borrowedId
        let Borrowed borrowersCount borrowPositions = variableState variable
        let state' = if borrowersCount == 1 then Free
                else Borrowed (borrowersCount - 1) (delete p borrowPositions)
        setVariableById borrowedId (setVariableState state' variable)
        return ()
    removeBorrowMut :: (VariableId, BNFC'Position) -> PreprocessorMonad ()
    removeBorrowMut borrow = do
        mutateVariableById (fst borrow) (setVariableState Free)
        return ()
