module Common.Types where

import Fe.Abs (Ident, BNFC'Position)
import Common.Utils (Identifier)

data Type =
    TUntyped |
    TPrimitive PrimitiveType |
    TStruct StructType |
    TVariant VariantType |
    TFunction FunctionType |
    TArray ArrayType |
    TReference ReferenceType
  deriving (Eq, Ord, Show, Read)

data PrimitiveType =
    I32 |
    Char |
    Bool |
    Unit
  deriving (Eq, Ord, Show, Read)

stringType :: Type
stringType = TArray $ Array (TPrimitive Char) UnSized

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

-- name, defined_at, [(field name, field type)]
data StructType = Struct Identifier [Field]
  deriving (Eq, Ord, Show, Read)

data Field = Field Identifier Type
  deriving (Eq, Ord, Show, Read)

-- name, defined_at
data VariantType = Variant Identifier [Type]
  deriving (Eq, Ord, Show, Read)

-- defined_at, function_kind, [params], return_type
data FunctionType = Function FunctionName FunctionKind Lifetime [Type] Type
  deriving (Eq, Ord, Show, Read)

data FunctionName = NamedFunction Identifier | Unnamed
  deriving (Eq, Ord, Show, Read)

data FunctionKind =
    Fn |
    FnOnce
  deriving (Eq, Ord, Show, Read)

data ArrayType = Array Type ArraySize
  deriving (Eq, Ord, Show, Read)

data ArraySize =
    Sized Int | -- TODO: change this to expression
    UnSized
  deriving (Eq, Ord, Show, Read)

data ReferenceType = Reference Lifetime Mutable Type
  deriving (Eq, Ord, Show, Read)

data Mutable = Mutable | Const
  deriving (Eq, Ord, Show, Read)

data Lifetime =
    Static |
    Scoped LifetimeName BNFC'Position Lifetime -- name, scope_begin, parent
  deriving (Eq, Ord, Show, Read)

data LifetimeName = Implicit | Explicit Ident
  deriving (Eq, Ord, Show, Read)
