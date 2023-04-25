module TypeCheck.Error where

import Data.Map
import Fe.Abs (BNFC'Position, Ident)
import Common.Types
import Common.Utils (Identifier)
import Common.Scope

data PreprocessorError =
    TypeAlreadyInScope Identifier Type Type | -- ident, new definition, old definition
    TypeNotDefined Identifier |
    TypeMismatch BNFC'Position Type Type | -- ident, ident actual type, ident expeced type
    VariableNotDefined Identifier |
    ConstantNotInitialized Identifier |
    VariableAtGlobalScope Identifier |
    UninitializedVariableUsed BNFC'Position Identifier | -- where, defined
    Other String BNFC'Position
  deriving (Eq, Ord, Show, Read)

data PreprocessorWarning =
    Shadow Identifier Identifier |
    Debug String
  deriving (Eq, Ord, Show, Read)
