module TypeCheck.Error where

import Data.Map

import Fe.Abs (BNFC'Position, Ident)

import Common.Types
import Common.Utils
import Common.Scope

import TypeCheck.Variable
import Common.Printer

data PreprocessorError =
    TypeAlreadyInScope Identifier Type Type | -- ident, new definition, old definition
    TypeNotDefined Identifier |
    TypeMismatch BNFC'Position Type Type | -- ident, ident actual type, ident expected type
    VariableNotDefined Identifier |
    ConstantNotInitialized Identifier |
    VariableAtGlobalScope Identifier |
    UninitializedVariableUsed BNFC'Position Identifier | -- where, defined
    CannotBorrow VariableId Identifier |
    LifetimesMismatch Lifetime Lifetime |
    CannotMakeEmptyReference BNFC'Position |
    InitializeConstantAsMutable Identifier Type |
    Other String BNFC'Position
  deriving (Eq, Ord, Show, Read)

data PreprocessorWarning =
    Shadow Identifier Identifier |
    Debug String
  deriving (Eq, Ord, Show, Read)

instance CodePrint PreprocessorWarning where
  codePrint tabs (Debug s) = "Warning: " ++ s
  codePrint tabs x = show x
