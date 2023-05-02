module Common.Types where

import Fe.Abs (Ident, BNFC'Position)
import Common.Utils

data Type =
    TUntyped |
    TPrimitive PrimitiveType |
    TStruct StructType |
    TVariant VariantType |
    TFunction FunctionType |
    TArray Type |
    TReference Mutable Type
  deriving (Eq, Ord, Show, Read)

data PrimitiveType =
    I32 |
    Char |
    Bool |
    Unit
  deriving (Eq, Ord, Show, Read)

stringType :: Type
stringType = arrayType charType

arrayType :: Type -> Type
arrayType = TArray

i32Type :: Type
i32Type = TPrimitive I32

charType :: Type
charType = TPrimitive Char

boolType :: Type
boolType = TPrimitive Bool

unitType :: Type
unitType = TPrimitive Unit

isInteger :: Type -> Bool
isInteger (TPrimitive I32) = True
isInteger _ = False

isPrimitive :: Type -> Bool
isPrimitive (TPrimitive _) = True
isPrimitive _ = False

isStruct :: Type -> Bool
isStruct (TStruct _) = True
isStruct _ = False

isVariant :: Type -> Bool
isVariant (TVariant _) = True
isVariant _ = False

isArray :: Type -> Bool
isArray (TArray _) = True
isArray _ = False

isFunction :: Type -> Bool
isFunction (TFunction _) = True
isFunction _ = False

isNamedFunction :: Type -> Bool
isNamedFunction (TFunction (Function (NamedFunction _) _ _ _)) = True
isNamedFunction _ = False

isClosure :: Type -> Bool
isClosure (TFunction (Function Unnamed _ _ _)) = True
isClosure _ = False

isReference :: Type -> Bool
isReference (TReference _ _) = True
isReference _ = False

isConstReference :: Type -> Bool
isConstReference (TReference Const _) = True
isConstReference _ = False

isMutReference :: Type -> Bool
isMutReference (TReference Mutable _) = True
isMutReference _ = False

isMoveableOnVariableExpression :: Type -> Bool
isMoveableOnVariableExpression t = isStruct t || isVariant t || isArray t || isReference t

-- name, defined_at, [(field name, field type)]
data StructType = Struct Identifier [Field]
  deriving (Eq, Ord, Show, Read)

data Field = Field Identifier Type
  deriving (Eq, Ord, Show, Read)

-- name, defined_at
data VariantType = Variant Identifier [Type]
  deriving (Eq, Ord, Show, Read)

-- defined_at, function_kind, [params], return_type
data FunctionType = Function FunctionName FunctionKind [FunctionParam] Type
  deriving (Eq, Ord, Show, Read)

data FunctionName = NamedFunction Identifier | Unnamed
  deriving (Eq, Ord, Show, Read)

data FunctionParam = FunctionParam Identifier Type
  deriving (Eq, Ord, Show, Read)

data FunctionKind =
    Fn |
    FnOnce
  deriving (Eq, Ord, Show, Read)

data Mutable = Mutable | Const
  deriving (Eq, Ord, Show, Read)
