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
