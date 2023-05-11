module TypeCheck.ValueUtils where

import Fe.Abs (BNFC'Position)

import Common.Types

import TypeCheck.State
import TypeCheck.Variable
import TypeCheck.VariablesUtils
import TypeCheck.BorrowCheckerUtils
import TypeCheck.Error

makeValue :: BNFC'Position -> Type -> ValueOwnership -> PreprocessorMonad Value
makeValue p TUntyped owned = do
    return $ Value TUntyped [] [] [] owned
makeValue p (TPrimitive t) owned = do
    return $ Value (TPrimitive t) [] [] [] owned
makeValue p (TStruct fields) owned = do
    throw $ Other "Structs not yet implemented" p
    -- return $ Value p (TStruct name fields) [] [] [] owned
makeValue p (TVariant types) owned = do
    throw $ Other "Variants not yet implemented" p
    -- return $ Value p (TVariant name types) [] [] [] owned
makeValue p (TFunction kind captures params ret) owned = do
    return $ Value (TFunction kind captures params ret) [] [] [] owned -- does not add captures to the value
makeValue p (TArray t) owned = do
    inner <- makeValue p t ByVariable
    innerPlace <- addTemporaryVariable Mutable inner
    return $ Value (TArray t) [innerPlace] [] [] owned
makeValue p (TReference Const t) owned = do
    inner <- makeValue p t ByVariable
    innerPlace <- addTemporaryVariable Const inner
    borrow (innerPlace, p)
    return $ Value (TReference Const t) [] [(innerPlace, p)] [] owned
makeValue p (TReference Mutable t) owned = do
    inner <- makeValue p t ByVariable
    innerPlace <- addTemporaryVariable Mutable inner
    borrowMut (innerPlace, p)
    return $ Value (TReference Mutable t) [] [] [(innerPlace, p)] owned

setValueType :: Type -> Value -> Value
setValueType t (Value _ ownedPlaces borrows borrowsMut owned) =
    Value t ownedPlaces borrows borrowsMut owned
