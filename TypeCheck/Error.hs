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
    ExpressionNotCallable BNFC'Position Type |
    NotPlaceExpression BNFC'Position |
    VariableNotDefined Identifier |
    ConstantNotInitialized Identifier |
    VariableAtGlobalScope Identifier |
    UninitializedVariableUsed BNFC'Position Identifier | -- where, defined
    UseAfterMoved BNFC'Position VariableId |
    AssignmentToConstant BNFC'Position VariableId |
    AlreadyBorrowed VariableId BNFC'Position |
    CannotMoveOut Variable |
    LifetimesMismatch BNFC'Position BNFC'Position  Lifetime Lifetime | -- if position is nothing -> lifetime is static
    CannotMakeEmptyReference BNFC'Position |
    WrongNumberOfParams BNFC'Position Type |
    Other String BNFC'Position |
    Fatal String
  deriving (Eq, Ord, Show, Read)

data PreprocessorWarning =
    Shadow Identifier Identifier |
    VariableNotInitializedNotUsed Identifier |
    Debug String
  deriving (Eq, Ord, Show, Read)

instance CodePrint PreprocessorWarning where
  codePrint tabs (Debug s) = "Warning: " ++ s
  codePrint tabs x = show x
