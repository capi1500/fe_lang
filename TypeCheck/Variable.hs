module TypeCheck.Variable where

import Common.Utils
import Common.Types
import Common.Printer

import Data.Maybe
import Data.Set

import Fe.Abs (Ident(..), BNFC'Position)

type VariableId = Int

data VariableState =
    Uninitialized |
    Borrowed Int (Set BNFC'Position) |
    BorrowedMut BNFC'Position |
    Moved |
    Free
  deriving (Eq, Ord, Show, Read)

-- potrzebuje:
--  - łatwego wypisywanie odkąd dokąd trwa lifetime
--  - porwnywania, czy jeden jest podlifetimem drugiego
data Lifetime = Lifetime [Int] Int -- lifetime predecessors ids list, (where begins, where ends) (for printing)
  deriving (Eq, Ord, Show, Read)

data ValueOwnership = ByVariable | ByExpression | ByBorrow VariableId
  deriving (Eq, Ord, Show, Read)

isOwnershipByBorrow :: ValueOwnership -> Bool
isOwnershipByBorrow (ByBorrow _) = True
isOwnershipByBorrow _ = False

data Value = Value {
    valueType :: Type,
    ownedPlaces :: [VariableId], -- now only for arrays: first varId contains info about all indices
    borrows :: [(VariableId, BNFC'Position)],
    borrowsMut :: [(VariableId, BNFC'Position)],
    owned :: ValueOwnership
} deriving (Eq, Ord, Show, Read)

data Variable = Variable {
    variableCreatedAt :: BNFC'Position,
    variableName :: Maybe Identifier,
    variableType :: Type,
    variableId :: VariableId,
    variableMutability :: Mutable,
    variableState :: VariableState,
    variableValue :: Value,
    lifetime :: Lifetime
} deriving (Eq, Ord, Show, Read)

isBorrowed :: VariableState -> Bool
isBorrowed (Borrowed _ _) = True
isBorrowed _ = False

setVariableState :: VariableState -> Variable -> Variable
setVariableState variableState (Variable createdAt variableIdentifier id variableType const _ value lifetime) =
    Variable createdAt variableIdentifier id variableType const variableState value lifetime

setVariableValue :: Value -> Variable -> Variable
setVariableValue value (Variable createdAt variableIdentifier variableType id const variableState _ lifetime) =
    Variable createdAt variableIdentifier variableType id const variableState value lifetime

setVariableId :: VariableId -> Variable -> Variable
setVariableId id (Variable createdAt variableIdentifier variableType _ const variableState value lifetime) =
    Variable createdAt variableIdentifier variableType id const variableState value lifetime

setValueOwned :: ValueOwnership -> Value -> Value
setValueOwned owned (Value t ownedPlaces borrows borrowsMut _) =
    Value t ownedPlaces borrows borrowsMut owned

setOwnedPlaces :: [VariableId] -> Value -> Value
setOwnedPlaces ownedVariables (Value t _ borrows borrowsMut owned) =
    Value t ownedVariables borrows borrowsMut owned

addBorrowToValue :: (VariableId, BNFC'Position) -> Value -> Value
addBorrowToValue borrow (Value t ownedVariables borrows borrowsMut owned) =
    Value t ownedVariables (borrow:borrows) borrowsMut owned

addMutBorrowToValue :: (VariableId, BNFC'Position) -> Value -> Value
addMutBorrowToValue borrow (Value t ownedVariables borrows borrowsMut owned) =
    Value t ownedVariables borrows (borrow:borrowsMut) owned
