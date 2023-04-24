module TypeCheck.Error where

import Fe.Abs (BNFC'Position, Ident)
import Common.Types
import Common.Utils (Identifier)

data PreprocessorError =
    TypeAlreadyInScope Identifier Type Type | -- ident, new definition, old definition
    TypeNotDefined Identifier |
    TypeMismatch Identifier Type Type | -- ident, ident actual type, ident expeced type
    VariableNotDefined Identifier |
    VariableAtGlobalScope Identifier |
    Other String BNFC'Position
  deriving (Eq, Ord, Show, Read)
