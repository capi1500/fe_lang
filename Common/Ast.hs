{-# LANGUAGE FlexibleInstances #-}

module Common.Ast where

import Data.Map (Map)
import Data.List (intercalate)

import Common.Types
import Common.Utils
import Common.Printer

mainFunction :: Identifier
mainFunction =  "main"

type Pointer = Int

newtype Code = Code [Statement]
  deriving (Eq, Ord, Show, Read)

data Statement =
    EmptyStatement |
    TypeStatement Type |
    NewVariableStatement Identifier Initialization | -- ident, is_reference, init
    NewFunctionStatement Identifier Expression [Identifier] |
    ExpressionStatement Expression
  deriving (Eq, Ord, Show, Read)

data Initialization = VarInitialized Expression | VarUninitialized
  deriving (Eq, Ord, Show, Read)

data Expression =
    BlockExpression [Statement] |
    IfExpression Expression Expression (Maybe Expression) | -- condition onTrue [onFalse]
    -- WhileExpression Expression Expression |
    -- ForExpression Pattern Expression Expression |
    -- MatchExpression Expression [MatchArm]
    LiteralExpression Value |
    MakeArrayExpression [Value] |
    VariableExpression Identifier | -- ident, isRef
    ReferenceExpression Identifier |
    DereferenceExpression Expression |
    -- StructExpression Ident [StructExpressionField]
    -- ArrayExpressionItems [ArrayElement]
    -- ArrayExpressionDefault Expression Expression |
    -- ClousureExpression [Capture] [FunctionParam] FunctionReturnType Expression |
    -- FieldExpression Expression Ident |
    CallExpression Expression [(Identifier, Expression)] | -- ident, is_reference, expression
    -- IndexExpression Expression Expression |
    -- UnaryExpression UnaryOperator Expression |
    UnaryMinusExpression Expression |
    UnaryNegationExpression Expression |
    -- TypeCastExpression Expression (Type) |
    I32DoubleOperatorExpression NumericDoubleOperator Expression Expression |
    BoolDoubleOperatorExpression BooleanDoubleOperator Expression Expression |
    -- ComparisonExpression Expression ComparisonOperator Expression |
    -- LazyAndExpression Expression Expression |
    -- LazyOrExpression Expression Expression |
    -- RangeExpression Expression Expression |
    AssignmentExpression Expression Expression -- expression1, expression2
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
    VFunction [Pointer] Expression | -- captures, code
    VArray [Pointer] | -- values
    VReference Pointer
  deriving (Eq, Ord, Show, Read)

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
