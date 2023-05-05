module TypeCheck.Variable where

import Data.Set (Set)

import Common.Utils
import Common.Types
import Fe.Abs (Ident(..))
import Common.Printer

type VariableId = Int

data VariableState =
    Uninitialized |
    Borrowed (Set VariableId) |
    BorrowedMut VariableId |
    Moved |
    Free
  deriving (Eq, Ord, Show, Read)


-- potrzebuje:
--  - łatwego wypisywanie odkąd dokąd trwa lifetime
--  - porwnywania, czy jeden jest podlifetimem drugiego
data Lifetime = Lifetime [Int] Int -- lifetime predecessors ids list, (where begins, where ends) (for printing)
  deriving (Eq, Ord, Show, Read)


data Variable = Variable {
    variableIdentifier :: Identifier,
    variableMutability :: Mutable,
    variableType :: Type,
    variableState :: VariableState,
    borrows :: [VariableId],
    borrowsMut :: [VariableId],
    lifetime :: Lifetime
} deriving (Eq, Ord, Show, Read)

isBorrowed :: VariableState -> Bool
isBorrowed (Borrowed _) = True
isBorrowed _ = False

instance CodePrint Variable where
  codePrint tabs (Variable (Identifier p value) mutability t state borrows borrowsMut lifetime) =
    let Lifetime list _ = lifetime in
    printTabs tabs ++ "{\n" ++
    printTabs (tabs + 1) ++ show value ++ ": " ++ (if isConst mutability then "const " else "") ++ codePrint tabs t ++ "\n" ++
    printTabs (tabs + 1) ++ show state ++ "\n" ++
    printTabs (tabs + 1) ++ "borrows: " ++ codePrint tabs borrows ++ "\n" ++
    printTabs (tabs + 1) ++ "borrowsMut: " ++ codePrint tabs borrowsMut ++ "\n" ++
    printTabs (tabs + 1) ++ "lifetime: " ++ codePrint tabs list ++ "\n" ++
    printTabs tabs ++ "}"


setVariableState :: VariableState -> Variable -> Variable
setVariableState variableState (Variable variableIdentifier variableType const _ borrows borrowsMut lifetime) =
    Variable variableIdentifier variableType const variableState borrows borrowsMut lifetime

setVariableBorrows :: [VariableId] -> Variable -> Variable
setVariableBorrows borrows (Variable variableIdentifier variableType const variableState _ borrowsMut lifetime) =
    Variable variableIdentifier variableType const variableState borrows borrowsMut lifetime

setVariableBorrowsMut :: [VariableId] -> Variable -> Variable
setVariableBorrowsMut borrowsMut (Variable variableIdentifier variableType const variableState borrows _ lifetime) =
    Variable variableIdentifier variableType const variableState borrows borrowsMut lifetime

changeVariable :: VariableState -> [VariableId] -> [VariableId] -> Variable -> Variable
changeVariable variableState borrows borrowsMut (Variable variableIdentifier variableType const _ _ _ lifetime) =
    Variable variableIdentifier variableType const variableState borrows borrowsMut lifetime