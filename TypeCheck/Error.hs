module TypeCheck.Error where

import Data.Map

import Fe.Abs (BNFC'Position, Ident)

import Common.Types
import Common.Utils
import Common.Scope

import TypeCheck.Variable
import Common.Printer

data PreprocessorError =
    TypeAlreadyInScope BNFC'Position Identifier Type Type | -- ident, new definition, old definition
    TypeNotDefined BNFC'Position Identifier |
    TypeMismatch BNFC'Position Type Type | -- ident, ident actual type, ident expected type
    ExpressionNotCallable BNFC'Position Type |
    IllegalInLValue BNFC'Position |
    IllegalInRValue BNFC'Position |
    VariableNotDefined BNFC'Position Identifier |
    ConstantNotInitialized BNFC'Position Identifier |
    VariableAtGlobalScope BNFC'Position Identifier |
    UninitializedVariableUsed BNFC'Position Identifier | -- where, defined
    UseAfterMoved BNFC'Position VariableId |
    AssignmentToConstant BNFC'Position VariableId |
    AlreadyBorrowed BNFC'Position VariableId |
    CannotMoveOut Variable |
    LifetimesMismatch BNFC'Position BNFC'Position Lifetime Lifetime | -- if position is nothing -> lifetime is static
    CannotMakeEmptyReference BNFC'Position |
    CannotTakeMutableReferenceToConstant BNFC'Position VariableId |
    CannotDerefNotReference BNFC'Position Type |
    CannotDerefReferenceToMultipleVariables BNFC'Position |
    WrongNumberOfParams BNFC'Position Type |
    Other String BNFC'Position |
    CannotComapreFunctions Type |
    Fatal String
  deriving (Eq, Ord, Show, Read)

data PreprocessorWarning =
    Shadow Identifier VariableId |
    VariableNotInitializedNotUsed VariableId |
    Debug String
  deriving (Eq, Ord, Show, Read)

instance CodePrint PreprocessorWarning where
  codePrint tabs (Debug s) = "Warning: " ++ s
  codePrint tabs x = show x
