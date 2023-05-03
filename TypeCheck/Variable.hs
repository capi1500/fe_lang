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
    variableType :: Type,
    variableState :: VariableState,
    borrows :: [VariableId],
    borrowsMut :: [VariableId],
    lifetime :: Lifetime
} deriving (Eq, Ord, Show, Read)

isBorrowed :: VariableState -> Bool
isBorrowed (Borrowed _) = True
isBorrowed _ = False

instance CodePrint Type where
    codePrint _ t = show t

instance CodePrint Variable where
  codePrint tabs (Variable (Identifier p value) t state borrows borrowsMut lifetime) =
    let Lifetime list _ = lifetime in
    printTabs tabs ++ "{\n" ++
    printTabs (tabs + 1) ++ show value ++ ": " ++ codePrint tabs t ++ "\n" ++
    printTabs (tabs + 1) ++ show state ++ "\n" ++
    printTabs (tabs + 1) ++ "borrows: " ++ codePrint tabs borrows ++ "\n" ++
    printTabs (tabs + 1) ++ "borrowsMut: " ++ codePrint tabs borrowsMut ++ "\n" ++
    printTabs (tabs + 1) ++ "lifetime: " ++ codePrint tabs list ++ "\n" ++
    printTabs tabs ++ "}"


setVariableState :: VariableState -> Variable -> Variable
setVariableState variableState (Variable variableIdentifier variableType _ borrows borrowsMut lifetime) =
    Variable variableIdentifier variableType variableState borrows borrowsMut lifetime

setVariableBorrows :: [VariableId] -> Variable -> Variable
setVariableBorrows borrows (Variable variableIdentifier variableType variableState _ borrowsMut lifetime) =
    Variable variableIdentifier variableType variableState borrows borrowsMut lifetime

setVariableBorrowsMut :: [VariableId] -> Variable -> Variable
setVariableBorrowsMut borrowsMut (Variable variableIdentifier variableType variableState borrows _ lifetime) =
    Variable variableIdentifier variableType variableState borrows borrowsMut lifetime

changeVariable :: VariableState -> [VariableId] -> [VariableId] -> Variable -> Variable
changeVariable variableState borrows borrowsMut (Variable variableIdentifier variableType _ _ _ lifetime) =
    Variable variableIdentifier variableType variableState borrows borrowsMut lifetime
