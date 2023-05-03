module Common.Types where

import Fe.Abs (Ident, BNFC'Position)
import Common.Utils

data Type =
    TUntyped |
    TPrimitive PrimitiveType |
    TStruct Identifier [Field] |
    TVariant Identifier [Type] |
    TFunction FunctionName FunctionKind [FunctionParam] Type |
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
isStruct (TStruct _ _) = True
isStruct _ = False

isVariant :: Type -> Bool
isVariant (TVariant _ _) = True
isVariant _ = False

isArray :: Type -> Bool
isArray (TArray _) = True
isArray _ = False

isFunction :: Type -> Bool
isFunction TFunction {} = True
isFunction _ = False

isNamedFunction :: Type -> Bool
isNamedFunction (TFunction (NamedFunction _) _ _ _) = True
isNamedFunction _ = False

isClosure :: Type -> Bool
isClosure (TFunction Unnamed _ _ _) = True
isClosure _ = False

isOnceFunction :: Type -> Bool
isOnceFunction (TFunction _ FnOnce _ _) = True
isOnceFunction _ = False

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

getStricterOfFunctionKinds :: FunctionKind -> FunctionKind -> FunctionKind
getStricterOfFunctionKinds FnOnce _ = FnOnce
getStricterOfFunctionKinds _ FnOnce = FnOnce
getStricterOfFunctionKinds Fn Fn = Fn

data Field = Field Identifier Type
  deriving (Eq, Ord, Show, Read)

data FunctionName = NamedFunction Identifier | Unnamed
  deriving (Eq, Ord, Show, Read)

data FunctionParam = FunctionParam Identifier Type
  deriving (Ord, Show, Read)

instance Eq FunctionParam where
  (==) (FunctionParam _ t1) (FunctionParam _ t2) = t1 == t2

data FunctionKind =
    Fn |
    FnOnce
  deriving (Eq, Ord, Show, Read)

data Mutable = Mutable | Const
  deriving (Eq, Ord, Show, Read)
