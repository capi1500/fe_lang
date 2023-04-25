{-# LANGUAGE FlexibleInstances #-}

module Common.Ast where

import Common.Types
import TypeCheck.State (Variable)
import Common.Utils
import Fe.Abs (Ident (..))
import Data.Map (Map)

mainFunction :: Identifier
mainFunction = Identifier Nothing (Ident "main")

newtype Code = Code [Statement]
  deriving (Eq, Ord, Show, Read)

data Statement =
    EmptyStatement |
    TypeStatement Type |
    NewVariableStatement Identifier VariableId Initialization |
    NewFunctionStatement Identifier VariableId Expression |
    ExpressionStatement TypedExpression
  deriving (Eq, Ord, Show, Read)

data Initialization = VarInitialized Expression | VarUninitialized
  deriving (Eq, Ord, Show, Read)

data TypedExpression = TypedExpression Expression Type
  deriving (Eq, Ord, Show, Read)

data Expression =
    BlockExpression [Statement] |
    -- IfExpression IfExpression |
    -- WhileExpression Expression Expression |
    -- ForExpression Pattern Expression Expression |
    -- MatchExpression Expression [MatchArm]
    LiteralExpression Value |
    VariableExpression VariableId |
    -- StructExpression Ident [StructExpressionField]
    -- ArrayExpressionItems [ArrayElement]
    -- ArrayExpressionDefault Expression Expression |
    -- ClousureExpression [Capture] [FunctionParam] FunctionReturnType Expression |
    -- FieldExpression Expression Ident |
    -- CallExpression Expression [CallParam] |
    -- IndexExpression Expression Expression |
    -- UnaryExpression UnaryOperator Expression |
    -- TypeCastExpression Expression (Type) |
    I32DoubleOperatorExpression NumericDoubleOperator Expression Expression
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


data Value =
    VI32 Int |
    VChar Char |
    VBool Bool |
    VUnit |
    VStruct (Map Ident Value) |
    VVariant VariableId Value | -- value_tag, value
    VFunction Expression | -- captures, code
    VArray Int [Value] | -- size, values
    VReference Ident
  deriving (Eq, Ord, Show, Read)
