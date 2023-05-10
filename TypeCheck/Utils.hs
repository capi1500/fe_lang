module TypeCheck.Utils where

import Data.Map ( empty, insert, fromList, lookup, Map )
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (intercalate, nub)
import Control.Monad.Except
import Control.Monad.State

import qualified Fe.Abs as A

import Common.Ast hiding (Uninitialized, Variable, position, Other, Value)
import Common.Types
import Common.Utils
import Common.Scope
import Common.Printer

import Fe.Abs (Ident, BNFC'Position)

import TypeCheck.State
import TypeCheck.Error
import TypeCheck.Variable
import TypeCheck.VariablesUtils
import TypeCheck.LifetimeUtils
import TypeCheck.Printer
import Data.Set (union, size)

assertType :: BNFC'Position -> Type -> Type -> PreprocessorMonad ()
assertType p actualType expectedType = do
    unless (canSubstitute actualType expectedType) $
        throw $ TypeMismatch p actualType expectedType

isVariableCV :: A.CV -> Bool
isVariableCV (A.Const _) = False
isVariableCV (A.Var _) = True

isConstCV :: A.CV -> Bool
isConstCV (A.Const _) = True
isConstCV (A.Var _) = False

isUnInitialized :: A.Initialization -> Bool
isUnInitialized (A.Initialized _ _) = False
isUnInitialized (A.UnInitialized _) = True

isInitialized :: A.Initialization -> Bool
isInitialized (A.Initialized _ _) = True
isInitialized (A.UnInitialized _) = False

-- TODO: deal with annotated lifetimes
modifyType :: A.TypeModifier -> Type -> PreprocessorMonad Type
modifyType (A.Ref _ lifetime) t = do
    lifetime' <- castLifetime lifetime
    return $ TReference Const t
modifyType (A.MutRef _ lifetime) t = do
    lifetime' <- castLifetime lifetime
    return $ TReference Mutable t

castLifetime :: A.Lifetime -> PreprocessorMonad Lifetime
castLifetime (A.ExplicitLifetime p ident) = do
    throw $ Other "Not yet implemented" p
castLifetime (A.ImplicitLifetime _) = do
    LifetimeState lifetime _ <- gets lifetimeState
    return lifetime

getShortestLifetime :: A.BNFC'Position -> [VariableId] -> PreprocessorMonad Lifetime
getShortestLifetime p variables = do
    when (null variables) $ throw (CannotMakeEmptyReference p)
    zipped <- traverse (\id -> do
        variable <- getVariableById id
        let position = variableCreatedAt variable
        return (lifetime variable, position)) variables
    let head:tail = zipped
    (l, p) <- foldM helper head tail
    return l
  where
    helper (l1, p1) (l2, p2) = do
        l <- getShorterOfLifetimesOrThrow p1 p2 l1 l2
        return $ if l == l1 then
                (l1, p1)
            else
                (l2, p2)

getShorterOfLifetimesOrThrow :: A.BNFC'Position -> A.BNFC'Position -> Lifetime -> Lifetime -> PreprocessorMonad Lifetime
getShorterOfLifetimesOrThrow p1 p2 first second = do
    if isSubLifetime first second then
        return second
    else if isSubLifetime second first then
        return first
    else
        throw $ LifetimesMismatch p1 p2 first second

-- assumes f1 and f2 are function types
mergeFunctionTypesOrThrow :: A.BNFC'Position -> Type -> Type -> PreprocessorMonad Type
mergeFunctionTypesOrThrow p f1 f2 = do
    let TFunction kind1 captures1 params1 returnType1 = f1
    let TFunction kind2 captures2 params2 returnType2 = f2
    when (params1 /= params2 || returnType1 /= returnType2) $ throw (TypeMismatch p f1 f2)
    return $ TFunction (getStricterOfFunctionKinds kind1 kind2) (mergeCaptures captures1 captures2) params1 returnType1
  where
    mergeCaptures captures1 captures2 = captures1 ++ captures2 -- TODO


mergeVariables :: [Variable] -> [Variable] -> PreprocessorMonad [Variable]
mergeVariables vars1 vars2 = do
    p <- gets position
    traverse (uncurry (mergeVariable p)) (zip vars1 vars2)
  where
    mergeVariable p v1 v2 = do
        when (variableId v1 /= variableId v2 && variableCreatedAt v1 /= variableCreatedAt v2) $ throw (Fatal "cannot merge variables in if")
        state <- mergeStates p (variableId v1) (variableState v1) (variableState v2)
        let value = mergeValues (variableValue v1) (variableValue v2)
        let vState = setVariableState state v1
        let vValue = mutateVariableValue (const value) vState
        return vValue

    mergeStates p id Uninitialized Uninitialized = return Uninitialized
    mergeStates p id Uninitialized s2 = throw $ CannotMergeStateAfterIf p id Uninitialized s2
    mergeStates p id s1 Uninitialized = throw $ CannotMergeStateAfterIf p id s1 Uninitialized
    mergeStates p id Moved Moved = return Moved
    mergeStates p id Moved s2 = throw $ CannotMergeStateAfterIf p id Moved s2
    mergeStates p id s1 Moved = throw $ CannotMergeStateAfterIf p id s1 Moved
    mergeStates p id (Borrowed x1 set1) (Borrowed x2 set2) = do
        let merged = set1 `union` set2
        return $ Borrowed (size merged) merged
    mergeStates p id (BorrowedMut p1) (BorrowedMut p2) = do
        unless (p1 == p2) $ throw (CannotMergeStateAfterIf p id (BorrowedMut p1) (BorrowedMut p2))
        return $ BorrowedMut p1
    mergeStates p id s1 Free = return s1
    mergeStates p id Free s2 = return s2
    mergeStates p id s1 s2 = throw $ CannotMergeStateAfterIf p id s1 s2

    mergeValues v1 v2 =
        Value {
            valueType = valueType v1,
            ownedPlaces = ownedPlaces v1,
            borrows = nub $ borrows v1 ++ borrows v2,
            borrowsMut = nub $ borrowsMut v1 ++ borrowsMut v2,
            owned = owned v1
        }


nameOfItem :: A.Item -> String
nameOfItem (A.ItemFunction _ (A.Ident ident) _ _ _ _) = ident
nameOfItem (A.ItemStruct _ (A.Ident ident) _ _) = ident
nameOfItem (A.ItemVariant _ (A.Ident ident) _ _) = ident
nameOfItem (A.ItemVariable _ _ (A.Ident ident) _ _) = ident

stripReferences :: Value -> Mutable -> PreprocessorMonad (Expression -> Expression, Value)
stripReferences value req_mut = do
    helper (valueType value) value
    where
        helper (TReference mut t) value = do
            do {
                derefed <- deref value;
                variable <- getVariableById derefed;
                (e, v) <- stripReferences (variableValue variable) req_mut;

                when (req_mut == Mutable && not (isReference (valueType v)) && mut == Const) $ throw ( CannotTakeMutableReferenceToConstant Nothing derefed);

                return (DereferenceExpression . e, v)
            } `catchError` handler
        helper t value = return (id, value)
        handler (CannotDerefReferenceToMultipleVariables _) = return (id, value)
        handler e = throw e


deref :: Value -> PreprocessorMonad VariableId
deref borrowedValue = do
    p <- gets position
    let borrows' = borrows borrowedValue
    let borrowsMut' = borrowsMut borrowedValue
    unless (length borrows' + length borrowsMut' == 1) $ throw (CannotDerefReferenceToMultipleVariables p)
    return $ fst (if null borrows' then
            head borrowsMut'
        else
            head borrows')
