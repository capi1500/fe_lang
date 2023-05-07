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

data Value = Value {
    valueCreatedAt :: BNFC'Position,
    valueType :: Type,
    borrows :: [VariableId],
    borrowsMut :: [VariableId],
    owned :: Bool
} deriving (Eq, Ord, Show, Read)

makeValue :: BNFC'Position -> Type -> Bool -> Value
makeValue p t = Value p t [] []

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

-- setVariableBorrows :: [VariableId] -> Variable -> Variable
-- setVariableBorrows borrows (Variable createdAt variableIdentifier id variableType const variableState _ borrowsMut lifetime) =
--     Variable createdAt variableIdentifier id variableType const variableState borrows borrowsMut lifetime

-- setVariableBorrowsMut :: [VariableId] -> Variable -> Variable
-- setVariableBorrowsMut borrowsMut (Variable createdAt variableIdentifier id variableType const variableState borrows _ lifetime) =
--     Variable createdAt variableIdentifier id variableType const variableState borrows borrowsMut lifetime

-- changeValue :: VariableState -> [VariableId] -> [VariableId] -> Variable -> Variable
-- changeValue variableState borrows borrowsMut (Variable createdAt variableIdentifier id variableType const _ _ _ lifetime) =
--     Value t borrows borrowsMut lifetime
  
-- setVariableLifetime :: Lifetime -> Variable -> Variable
-- setVariableLifetime lifetime (Variable createdAt variableIdentifier id variableType const variableState borrows borrowsMut _) =
--     Variable createdAt variableIdentifier id variableType const variableState borrows borrowsMut lifetime

mutateVariableValue :: (Value -> Value) -> Variable -> Variable
mutateVariableValue mutate (Variable createdAt variableIdentifier variableType id const variableState value lifetime) = 
    Variable createdAt variableIdentifier variableType id const variableState (mutate value) lifetime

setVariableId :: VariableId -> Variable -> Variable
setVariableId id (Variable createdAt variableIdentifier variableType _ const variableState value lifetime) =
    Variable createdAt variableIdentifier variableType id const variableState value lifetime

setValueOwned :: Bool -> Value -> Value
setValueOwned owned (Value createdAt t borrows borrowsMut _) = 
    Value createdAt t borrows borrowsMut owned
