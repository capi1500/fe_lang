{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Ast where

import Prelude (Integer)
import qualified Prelude as C (Eq, Ord, Show, Read)

import Common.Types
import qualified Fe.Abs as A

data Annotation =
    Position A.BNFC'Position |
    Typed A.BNFC'Position Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

class HasAnnotation a where
  getAnnotation :: a Annotation -> Annotation

instance HasAnnotation A.Code' where
  getAnnotation = \case
    A.Code p _ -> p

instance HasAnnotation A.Statement' where
  getAnnotation = \case
    A.SemicolonStatement p -> p
    A.ItemStatement p _ -> p
    A.ExpressionStatement p _ -> p

instance HasAnnotation A.Item' where
  getAnnotation = \case
    A.ItemFunction p _ -> p
    A.ItemStruct p _ -> p
    A.ItemVariant p _ -> p
    A.ItemVariable p _ -> p
    A.ItemConst p _ -> p

instance HasAnnotation A.Function' where
  getAnnotation = \case
    A.Function p _ _ _ _ -> p

instance HasAnnotation A.FunctionParam' where
  getAnnotation = \case
    A.Parameter p _ _ -> p

instance HasAnnotation A.FunctionReturnType' where
  getAnnotation = \case
    A.ReturnValue p _ -> p
    A.ReturnUnit p -> p

instance HasAnnotation A.Struct' where
  getAnnotation = \case
    A.Struct p _ _ -> p

instance HasAnnotation A.StructField' where
  getAnnotation = \case
    A.StructField p _ _ -> p

instance HasAnnotation A.Variant' where
  getAnnotation = \case
    A.Variant p _ _ -> p

instance HasAnnotation A.VariantItem' where
  getAnnotation = \case
    A.VariantItem p _ -> p

instance HasAnnotation A.Variable' where
  getAnnotation = \case
    A.Variable p _ -> p

instance HasAnnotation A.Const' where
  getAnnotation = \case
    A.Const p _ -> p

instance HasAnnotation A.CVDeclaration' where
  getAnnotation = \case
    A.CVDeclaration p _ _ _ -> p

instance HasAnnotation A.TypeDeclaration' where
  getAnnotation = \case
    A.Typed p _ -> p
    A.Untyped p -> p

instance HasAnnotation A.Type' where
  getAnnotation = \case
    A.TypeSimpleType p _ _ -> p
    A.TypeArrayType p _ _ -> p
    A.TypeFunctionType p _ -> p

instance HasAnnotation A.TypeModifier' where
  getAnnotation = \case
    A.None p -> p
    A.Ref p -> p
    A.RefLifetime p _ -> p
    A.MutRef p -> p
    A.MutRefLifetime p _ -> p

instance HasAnnotation A.Lifetime' where
  getAnnotation = \case
    A.Lifetime p _ -> p

instance HasAnnotation A.SimpleType' where
  getAnnotation = \case
    A.SimpleType p _ -> p

instance HasAnnotation A.ArrayType' where
  getAnnotation = \case
    A.ArrayTypeSized p _ _ -> p
    A.ArrayType p _ -> p

instance HasAnnotation A.FunctionType' where
  getAnnotation = \case
    A.FunctionType p _ _ -> p

instance HasAnnotation A.FunctionTypeParam' where
  getAnnotation = \case
    A.FunctionTypeParam p _ -> p

instance HasAnnotation A.FunctionTypeReturnType' where
  getAnnotation = \case
    A.FunctionTypeReturnType p _ -> p
    A.FunctionTypeReturnTypeUnit p -> p

instance HasAnnotation A.Expression' where
  getAnnotation = \case
    A.BlockExpression p _ -> p
    A.GroupedExpression p _ -> p
    A.IfExpression p _ -> p
    A.WhileExpression p _ _ -> p
    A.ForExpression p _ _ _ -> p
    A.MatchExpression p _ _ -> p
    A.LiteralExpression p _ -> p
    A.VariableExpression p _ -> p
    A.StructExpression p _ _ -> p
    A.ArrayExpressionItems p _ -> p
    A.ArrayExpressionDefault p _ _ -> p
    A.ClousureExpression p _ _ _ _ -> p
    A.FieldExpression p _ _ -> p
    A.CallExpression p _ _ -> p
    A.IndexExpression p _ _ -> p
    A.UnaryExpression p _ _ -> p
    A.TypeCastExpression p _ _ -> p
    A.MultiplyExpression p _ _ -> p
    A.DivideExpression p _ _ -> p
    A.ModuloExpression p _ _ -> p
    A.PlusExpression p _ _ -> p
    A.MinusExpression p _ _ -> p
    A.LShiftExpression p _ _ -> p
    A.RShiftExpression p _ _ -> p
    A.BitAndExpression p _ _ -> p
    A.BitXOrExpression p _ _ -> p
    A.BitOrExpression p _ _ -> p
    A.ComparisonExpression p _ _ _ -> p
    A.LazyAndExpression p _ _ -> p
    A.LazyOrExpression p _ _ -> p
    A.RangeExpression p _ _ -> p
    A.AssignmentExpression p _ _ _ -> p
    A.BreakExpression p -> p
    A.ContinueExpression p -> p
    A.ReturnExpressionUnit p -> p
    A.ReturnExpressionValue p _ -> p

instance HasAnnotation A.IfExpression' where
  getAnnotation = \case
    A.If p _ _ -> p
    A.IfElse p _ _ _ -> p
    A.IfElseIf p _ _ _ -> p

instance HasAnnotation A.MatchArm' where
  getAnnotation = \case
    A.MatchArm p _ _ -> p

instance HasAnnotation A.Literal' where
  getAnnotation = \case
    A.LiteralChar p _ -> p
    A.LiteralString p _ -> p
    A.LiteralInteger p _ -> p
    A.LiteralDouble p _ -> p
    A.LiteralBoolean p _ -> p

instance HasAnnotation A.StructExpressionField' where
  getAnnotation = \case
    A.StructExpressionField p _ _ -> p
    A.StructExpressionFieldDefault p _ -> p

instance HasAnnotation A.ArrayElement' where
  getAnnotation = \case
    A.ArrayElement p _ -> p

instance HasAnnotation A.Capture' where
  getAnnotation = \case
    A.Capture p _ _ -> p

instance HasAnnotation A.CallParam' where
  getAnnotation = \case
    A.CallParam p _ -> p

instance HasAnnotation A.UnaryOperator' where
  getAnnotation = \case
    A.UnaryMinus p -> p
    A.UnaryNegation p -> p
    A.Dereference p -> p
    A.Reference p -> p
    A.ReferenceMut p -> p

instance HasAnnotation A.AssignmentOperator' where
  getAnnotation = \case
    A.Assign p -> p
    A.PlusEqual p -> p
    A.MinusEqual p -> p
    A.MultiplyEqual p -> p
    A.DivideEqual p -> p
    A.ModuloEqual p -> p
    A.AndEqual p -> p
    A.OrEqual p -> p
    A.XorEqual p -> p
    A.LShiftEqual p -> p
    A.RShiftEqual p -> p

instance HasAnnotation A.ComparisonOperator' where
  getAnnotation = \case
    A.Equals p -> p
    A.NotEquals p -> p
    A.Greater p -> p
    A.Smaller p -> p
    A.GreaterEquals p -> p
    A.SmallerEquals p -> p

instance HasAnnotation A.Pattern' where
  getAnnotation = \case
    A.VariantPattern p _ _ -> p
    A.StructPattern p _ _ -> p
    A.AssignPattern p _ -> p
    A.AllPattern p -> p

instance HasAnnotation A.Boolean' where
  getAnnotation = \case
    A.BoolTrue p -> p
    A.BoolFalse p -> p
