{-# LANGUAGE FlexibleInstances #-}

module Common.Ast where

import Data.Map (Map)
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Except

import Common.Types
import Common.Utils
import Common.Printer
import Common.Scope

import Fe.Abs (BNFC'Position)

mainFunction :: Identifier
mainFunction =  "main"

data ExecutionError =
    DivisionByZero BNFC'Position |
    ShiftInvalidArgument BNFC'Position Int |
    InputFailed BNFC'Position |
    IndexOutOfRange BNFC'Position Int Int | -- where, index, size
    Break |
    Continue |
    Return Value |
    Other String |
    TypeCheckerFailed String

instance Show ExecutionError where
    show (DivisionByZero p) = "Division by zero at: " ++ show p
    show (ShiftInvalidArgument p arg) = "Invalid shift argument: " ++ show arg ++ ", at: " ++ show p
    show (InputFailed p) = "Input failed at: " ++ show p
    show (IndexOutOfRange p index size) = "Index: " ++ show index ++ " out of range [0, " ++ show size ++ "] at: " ++ show p
    show Break = "Break"
    show Continue = "Continue"
    show (Return value) = "Return"
    show (Other string) = "Other " ++ string
    show (TypeCheckerFailed string) = "FATAL: " ++ string

data Variable = Variable Value | Uninitialized

type VariableMappings = Scope (Map Identifier Pointer)
data ExecutionState = ExecutionState {
    variableMappings :: VariableMappings,
    variables :: [Variable],
    input :: [String],
    position :: BNFC'Position
}

type ExecutorMonad a = ExceptT ExecutionError (StateT ExecutionState IO) a

type Pointer = Int

newtype Code = Code [Statement]

data Statement =
    EmptyStatement |
    TypeStatement Type |
    NewVariableStatement Identifier Initialization | -- ident, is_reference, init
    NewFunctionStatement Identifier Expression [Identifier] |
    ExpressionStatement Expression

data Initialization = VarInitialized Expression | VarUninitialized

data Expression =
    BlockExpression [Statement] |
    IfExpression Expression Expression (Maybe Expression) | -- condition onTrue [onFalse]
    WhileExpression Expression Expression | -- condition, block
    -- ForExpression Pattern Expression Expression |
    -- MatchExpression Expression [MatchArm]
    InternalExpression (ExecutorMonad Value) |
    LiteralExpression Value |
    MakeArrayExpression [Expression] |
    MakeArrayDefaultsExpression Expression Expression | -- size, default
    MakeClosureExpression [Capture] [Identifier] Expression | -- params, expression
    VariableExpression Identifier | -- ident, isRef
    ReferenceExpression Expression |
    DereferenceExpression Expression |
    -- StructExpression Ident [StructExpressionField]
    -- FieldExpression Expression Ident |
    CallExpression BNFC'Position Expression [Expression] | -- ident, is_reference, expression
    IndexExpression BNFC'Position Expression Expression |
    UnaryMinusExpression Expression |
    UnaryNegationExpression Expression |
    -- TypeCastExpression Expression (Type) |
    I32DoubleOperatorExpression BNFC'Position NumericDoubleOperator Expression Expression |
    BoolDoubleOperatorExpression BooleanDoubleOperator Expression Expression |
    -- RangeExpression Expression Expression |
    AssignmentExpression Expression Expression | -- expression1, expression2
    BreakExpression |
    ContinueExpression |
    ReturnExpression Expression

data NumericDoubleOperator =
    Plus |
    Minus |
    Multiply |
    Divide |
    Modulo |
    LShift |
    RShift |
    BitOr |
    BitXor |
    BitAnd
  deriving (Eq, Ord, Show, Read)

data BooleanDoubleOperator =
    Equals |
    Greater |
    Smaller |
    LazyAnd |
    LazyOr
  deriving (Eq, Ord, Show, Read)

data Value =
    VI32 Int |
    VChar Char |
    VBool Bool |
    VUnit |
    VStruct (Map Identifier Pointer) |
    VVariant Int Pointer | -- value_tag, value
    VFunction [ValueCapture] [Identifier] Expression | -- params, code
    VArray [Pointer] | -- values
    VReference Pointer |
    VVariable Pointer Variable -- pointer to self, value

data ValueCapture = ValueCapture Identifier Value

isString :: [Value] -> Bool
isString = all isChar

isChar :: Value -> Bool
isChar (VChar _) = True
isChar _ = False
