module TypeCheck.ValueUtils where

import Fe.Abs (BNFC'Position)

import Common.Types

import TypeCheck.State
import TypeCheck.Variable
import TypeCheck.VariablesUtils
import TypeCheck.BorrowCheckerUtils
import TypeCheck.Error

makeValue :: BNFC'Position -> Type -> Bool -> PreprocessorMonad Value
makeValue p TUntyped owned = do
    return $ Value p TUntyped [] [] [] owned
makeValue p (TPrimitive t) owned = do
    return $ Value p (TPrimitive t) [] [] [] owned
makeValue p (TStruct fields) owned = do
    throw $ Other "Structs not yet implemented" p
    -- return $ Value p (TStruct name fields) [] [] [] owned
makeValue p (TVariant types) owned = do
    throw $ Other "Variants not yet implemented" p
    -- return $ Value p (TVariant name types) [] [] [] owned
makeValue p (TFunction kind params ret) owned = do
    return $ Value p (TFunction kind params ret) [] [] [] owned
makeValue p (TArray t) owned = do
    inner <- makeValue p t True
    innerPlace <- addTemporaryVariable Mutable inner
    return $ Value p (TArray t) [innerPlace] [] [] owned
makeValue p (TReference Const t) owned = do
    inner <- makeValue p t True
    innerPlace <- addTemporaryVariable Const inner
    borrow innerPlace
    return $ Value p (TReference Const t) [] [innerPlace] [] owned
makeValue p (TReference Mutable t) owned = do
    inner <- makeValue p t True
    innerPlace <- addTemporaryVariable Mutable inner
    borrowMut innerPlace
    return $ Value p (TReference Mutable t) [] [] [innerPlace] owned
