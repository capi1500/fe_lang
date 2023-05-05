module TypeCheck.Utils where

import Data.Map ( empty, insert, fromList, lookup, Map )
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad.Except
import Control.Monad.State

import qualified Fe.Abs as A

import Common.Ast
import Common.Types
import Common.Utils
import Common.Scope

import Fe.Abs (Ident)

import TypeCheck.State hiding (Static)
import TypeCheck.Error
import TypeCheck.Variable
import Data.List (intercalate)
import Common.Printer
import TypeCheck.StateFunctions

assertType :: Type -> Type -> A.BNFC'Position -> PreprocessorMonad ()
assertType actualType expectedType p = do
    when (expectedType /= actualType) $
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
modifyType :: Type -> A.TypeModifier -> PreprocessorMonad Type
modifyType t (A.None _) = do return t
modifyType t (A.Ref _ lifetime) = do
    lifetime' <- castLifetime lifetime
    return $ TReference Const t
modifyType t (A.MutRef _ lifetime) = do
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
        let Identifier position _ = variableIdentifier variable
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
    let TFunction name1 kind1 params1 returnType1 = f1
    let TFunction _ kind2 params2 returnType2 = f2
    when (params1 /= params2 || returnType1 /= returnType2) $ throw (TypeMismatch p f1 f2)
    return $ TFunction name1 (getStricterOfFunctionKinds kind1 kind2) params1 returnType1

nameOfItem :: A.Item -> String
nameOfItem (A.ItemFunction _ (A.Ident ident) _ _ _ _) = ident
nameOfItem (A.ItemStruct _ (A.Ident ident) _ _) = ident
nameOfItem (A.ItemVariant _ (A.Ident ident) _ _) = ident
nameOfItem (A.ItemVariable _ _ (A.Ident ident) _ _) = ident

-- tries to dereference a variable, which is currently marked as used
-- it can be only done after expression parsed in LValue context
-- Assumes:
--  - used variable is a 
--      - temporary referenece
--      - it mutably borrows a non-temporary variable
deref :: PreprocessorMonad (VariableId, Variable)
deref = do
    maybeUsed <- gets usedVariables
    let variableRefId = fromJust maybeUsed
    variableRef <- getVariableById variableRefId
    handleUsedVariables moveOutVariable

    let TReference _ t = variableType variableRef
    unless (null (borrows variableRef) && length (borrowsMut variableRef) == 1) $ throw (Fatal "LValue references more than 1 mutable reference")

    let variableId = head (borrowsMut variableRef)
    variable <- getVariableById variableId
    return (variableId, variable)
