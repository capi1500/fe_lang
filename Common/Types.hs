module Common.Types where

import Prelude (Integer, Maybe, Bool (..))
import qualified Prelude as C (Eq, Ord, Show, Read)
import Fe.Abs (Ident, BNFC'Position)

data Type =
    TUntyped |
    TPrimitive PrimitiveType |
    TStruct StructType |
    TVariant VariantType |
    TFunction FunctionType |
    TArray ArrayType |
    TReference ReferenceType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Mutable = Mutable | Const
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PrimitiveType =
    I32 |
    Char |
    Bool |
    Unit
  deriving (C.Eq, C.Ord, C.Show, C.Read)

isInteger :: Type -> Bool
isInteger (TPrimitive I32) = True
isInteger _ = False

-- name, defined_at, [(field name, field type)]
data StructType = Struct BNFC'Position [Field]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Field = Field Ident Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- name, defined_at
data VariantType = Variant BNFC'Position [Type]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- name, defined_at, [params], return_type
data FunctionType = Function BNFC'Position [Type] Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ArrayType = Array Type ArraySize
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ArraySize =
    Sized Integer |
    UnSized
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ReferenceType = Reference Lifetime Mutable Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Lifetime =
    Static |
    Scoped LifetimeName BNFC'Position Lifetime -- name, scope_begin, parent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data LifetimeName = Implicit | Explicit Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)
