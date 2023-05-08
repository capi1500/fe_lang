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

import Exec.Error
import Fe.Abs (BNFC'Position)

mainFunction :: Identifier
mainFunction =  "main"

data Variable = Variable Value | Uninitialized

type VariableMappings = Scope (Map Identifier Pointer)
data ExecutionState = ExecutionState {
    variableMappings :: VariableMappings,
    variables :: [Variable],
    input :: [String],
    position :: BNFC'Position
}

type ExecutorMonad a = StateT ExecutionState (ExceptT ExecutionError IO) a

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
    -- WhileExpression Expression Expression |
    -- ForExpression Pattern Expression Expression |
    -- MatchExpression Expression [MatchArm]
    InternalExpression (ExecutorMonad Value) |
    LiteralExpression Value |
    MakeArrayExpression [Expression] |
    MakeArrayDefaultsExpression Expression Expression | -- size, default
    VariableExpression Identifier | -- ident, isRef
    ReferenceExpression Identifier |
    DereferenceExpression Expression |
    -- StructExpression Ident [StructExpressionField]
    -- ArrayExpressionItems [ArrayElement]
    -- ArrayExpressionDefault Expression Expression |
    -- ClousureExpression [Capture] [FunctionParam] FunctionReturnType Expression |
    -- FieldExpression Expression Ident |
    CallExpression BNFC'Position Expression [Expression] | -- ident, is_reference, expression
    IndexExpression BNFC'Position Expression Expression |
    UnaryMinusExpression Expression |
    UnaryNegationExpression Expression |
    -- TypeCastExpression Expression (Type) |
    I32DoubleOperatorExpression BNFC'Position NumericDoubleOperator Expression Expression |
    BoolDoubleOperatorExpression BooleanDoubleOperator Expression Expression |
    -- LazyAndExpression Expression Expression |
    -- LazyOrExpression Expression Expression |
    -- RangeExpression Expression Expression |
    AssignmentExpression Expression Expression -- expression1, expression2
    -- BreakExpression |
    -- ContinueExpression |
    -- ReturnExpressionUnit |
    -- ReturnExpressionValue Expression

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
    VFunction [Identifier] Expression | -- params, code
    VArray [Pointer] | -- values
    VReference Pointer

isString :: [Value] -> Bool
isString = all isChar

isChar :: Value -> Bool
isChar (VChar _) = True
isChar _ = False

-- isPrimitiveValue :: Value -> Bool
-- isPrimitiveValue (VI32 _) = True
-- isPrimitiveValue (VChar _) = True
-- isPrimitiveValue (VBool _) = True
-- isPrimitiveValue VUnit = True
-- isPrimitiveValue (VArray _ _) = True
-- isPrimitiveValue _ = False
