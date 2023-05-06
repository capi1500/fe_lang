module TypeCheck.Variable where

import Data.Set (Set)

import Common.Utils
import Common.Types
import Common.Printer
import Fe.Abs (Ident(..), BNFC'Position)
import Data.Maybe

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

data Value = Value {
    valueType :: Type,
    borrows :: [VariableId],
    borrowsMut :: [VariableId],
    lifetime :: Lifetime
} deriving (Eq, Ord, Show, Read)

makeValue :: Type -> Lifetime -> Value
makeValue t = Value t [] []

data Variable = Variable {
    createdAt :: BNFC'Position,
    variableName :: Maybe Identifier,
    variableType :: Type,
    variableId :: VariableId,
    variableMutability :: Mutable,
    variableState :: VariableState,
    variableValue :: Value
} deriving (Eq, Ord, Show, Read)

isBorrowed :: VariableState -> Bool
isBorrowed (Borrowed _) = True
isBorrowed _ = False

instance CodePrint Value where
    codePrint tabs (Value t borrows borrowsMut lifetime) = 
        let Lifetime list _ = lifetime in
        "Value {\n" ++ 
        printTabs (tabs + 1) ++ "borrows" ++ codePrint tabs borrows ++ "\n" ++
        printTabs (tabs + 1) ++ "borrowsMut: " ++ codePrint tabs borrowsMut ++ "\n" ++
        printTabs (tabs + 1) ++ "lifetime: " ++ codePrint tabs list ++ "\n" ++
        printTabs tabs ++ "}"

instance CodePrint Variable where
    codePrint tabs (Variable createdAt maybeIdent t id mutability state value) =
        let name = fromMaybe "temporary" maybeIdent in
        "Variable " ++ show id ++ " {\n" ++
        printTabs (tabs + 1) ++ show name ++ ": " ++ (if isConst mutability then "const " else "") ++ codePrint tabs t ++ "\n" ++
        printTabs (tabs + 1) ++ "Value: " ++ codePrint (tabs + 1) value ++
        printTabs (tabs + 1) ++ show state ++ "\n" ++
        printTabs (tabs + 1) ++ "Created at: " ++ show createdAt ++ "\n" ++
        printTabs tabs ++ "}"

-- setVariableState :: VariableState -> Variable -> Variable
-- setVariableState variableState (Variable createdAt variableIdentifier id variableType const _ borrows borrowsMut lifetime) =
--     Variable createdAt variableIdentifier id variableType const variableState borrows borrowsMut lifetime

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

getVariablesValueLifetime :: Variable -> Lifetime
getVariablesValueLifetime variable = lifetime (variableValue variable)

setVariableId :: VariableId -> Variable -> Variable
setVariableId id (Variable createdAt variableIdentifier variableType _ const variableState value) =
    Variable createdAt variableIdentifier variableType id const variableState value