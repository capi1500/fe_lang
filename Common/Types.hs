module Common.Types where

import Fe.Abs (Ident, BNFC'Position)
import Common.Utils
import Common.Printer
import Data.List (intercalate)

data Type =
    TUntyped |
    TPrimitive PrimitiveType |
    TStruct [Field] |
    TVariant [Type] |
    TFunction FunctionKind [Capture] [Type] Type |
    TArray Type |
    TReference Mutable Type
  deriving (Eq, Ord, Show, Read)

-- can type1 be used as type2
canSubstitute :: Type -> Type -> Bool
canSubstitute (TFunction Fn _ params1 return1) (TFunction _ _ params2 return2) =
    all (uncurry canSubstitute) (zip params1 params2) && canSubstitute return1 return2
canSubstitute (TFunction FnOnce _ params1 return1) (TFunction FnOnce _ params2 return2) =
    all (uncurry canSubstitute) (zip params1 params2) && canSubstitute return1 return2
canSubstitute (TReference Mutable t1) (TReference _ t2) = canSubstitute t1 t2
canSubstitute (TReference Const t1) (TReference Const t2) = canSubstitute t1 t2
canSubstitute t1 t2 = t1 == t2

data PrimitiveType =
    I32 |
    Char |
    Bool |
    Unit
  deriving (Eq, Ord, Show, Read)

data Capture = Capture Identifier CaptureModifier
  deriving (Eq, Ord, Show, Read)

data CaptureModifier = CMRef Mutable | CMNone
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
isFunction TFunction {} = True
isFunction _ = False

isNamedFunction :: Type -> Bool
isNamedFunction (TFunction _ [] _ _) = True
isNamedFunction _ = False

-- isClosure :: Type -> Bool
-- isClosure (TFunction Unnamed _ _ _) = True
-- isClosure _ = False

isOnceFunction :: Type -> Bool
isOnceFunction (TFunction FnOnce _ _ _) = True
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

isCopy :: Type -> Bool
isCopy (TPrimitive _) = True
isCopy t = isNamedFunction t

data Field = Field Identifier Type
  deriving (Eq, Ord, Show, Read)

data FunctionName = NamedFunction Identifier | Unnamed
  deriving (Eq, Ord, Show, Read)

data FunctionKind =
    Fn |
    FnOnce
  deriving (Eq, Ord, Show, Read)

data Mutable = Mutable | Const
  deriving (Eq, Ord, Show, Read)

isConst :: Mutable -> Bool
isConst Const = True
isConst Mutable = False

isMutable :: Mutable -> Bool
isMutable Const = False
isMutable Mutable = True

instance CodePrint Type where
    codePrint tabs TUntyped = "Untyped"
    codePrint tabs (TPrimitive p) = show p
    codePrint tabs (TStruct _) = "struct"
    codePrint tabs (TVariant _) = "variant"
    codePrint tabs (TFunction kind captures params ret) = show kind ++ "(" ++ intercalate "," (fmap (codePrint tabs) params) ++ ") -> " ++ codePrint tabs ret
    codePrint tabs (TArray t) = "[" ++ codePrint tabs t ++ "]"
    codePrint tabs (TReference Const t) = "&" ++ codePrint tabs t
    codePrint tabs (TReference Mutable t) = "&mut " ++ codePrint tabs t

instance CodePrint FunctionName where
    codePrint tabs (NamedFunction ident) = " " ++ ident
    codePrint tabs Unnamed = ""
