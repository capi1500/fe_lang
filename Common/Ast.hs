{-# LANGUAGE FlexibleInstances #-}

module Common.Ast where

import Data.Map (Map)
import Data.List (intercalate)

import Fe.Abs (Ident (..))

import Common.Types
import Common.Utils
import Common.Printer

import TypeCheck.State
import TypeCheck.Variable (Lifetime)


mainFunction :: Identifier
mainFunction = Identifier Nothing (Ident "main")

newtype Pointer = Pointer Int
  deriving (Eq, Ord, Show, Read)

newtype Code = Code [Statement]
  deriving (Eq, Ord, Show, Read)

data Statement =
    EmptyStatement |
    TypeStatement Type |
    NewVariableStatement Identifier Ident Initialization |
    NewFunctionStatement Identifier Ident Expression [Ident] |
    ExpressionStatement TypedExpression
  deriving (Eq, Ord, Show, Read)

data Initialization = VarInitialized Expression | VarUninitialized
  deriving (Eq, Ord, Show, Read)

data TypedExpression = TypedExpression Expression Type Lifetime -- expression, type of expression, lifetime of value that the expression holds
  deriving (Eq, Ord, Show, Read)

data Expression =
    BlockExpression [Statement] |
    IfExpression Expression Expression (Maybe Expression) | -- condition onTrue [onFalse]
    -- WhileExpression Expression Expression |
    -- ForExpression Pattern Expression Expression |
    -- MatchExpression Expression [MatchArm]
    LiteralExpression Value |
    MakeArrayExpression [Value] |
    MoveExpression Expression | -- holds VMovedValue variableId
    VariableExpression Ident | -- variableId is valid for current frame
    GetReferenceExpression Expression | -- makes reference out of identifier
    -- StructExpression Ident [StructExpressionField]
    -- ArrayExpressionItems [ArrayElement]
    -- ArrayExpressionDefault Expression Expression |
    -- ClousureExpression [Capture] [FunctionParam] FunctionReturnType Expression |
    -- FieldExpression Expression Ident |
    CallExpression Expression [Expression] |
    -- IndexExpression Expression Expression |
    -- UnaryExpression UnaryOperator Expression |
    UnaryMinusExpression Expression |
    -- TypeCastExpression Expression (Type) |
    I32DoubleOperatorExpression NumericDoubleOperator Expression Expression |
    BoolDoubleOperatorExpression BooleanDoubleOperator Expression Expression
    -- ComparisonExpression Expression ComparisonOperator Expression |
    -- LazyAndExpression Expression Expression |
    -- LazyOrExpression Expression Expression |
    -- RangeExpression Expression Expression |
    -- AssignmentExpression Expression AssignmentOperator Expression |
    -- BreakExpression |
    -- ContinueExpression |
    -- ReturnExpressionUnit |
    -- ReturnExpressionValue Expression
  deriving (Eq, Ord, Show, Read)

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
    NotEquals |
    Greater |
    Smaller |
    GreaterEquals |
    SmallerEquals |
    LazyAnd |
    LazyOr
  deriving (Eq, Ord, Show, Read)

data Value =
    VI32 Int |
    VChar Char |
    VBool Bool |
    VUnit |
    VStruct (Map Ident Pointer) |
    VVariant Int Pointer | -- value_tag, value
    VFunction [Pointer] Expression | -- captures, code
    VArray Int [Pointer] | -- size, values
    VMovedValue Pointer |
    VReference Pointer
  deriving (Eq, Ord, Show, Read)

isPrimitiveValue :: Value -> Bool
isPrimitiveValue (VI32 _) = True
isPrimitiveValue (VChar _) = True
isPrimitiveValue (VBool _) = True
isPrimitiveValue VUnit = True
isPrimitiveValue (VArray _ _) = True
isPrimitiveValue _ = False
