module TypeCheck.Utils where

import Data.Map ( empty, insert, fromList, lookup, Map )
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad.Except
import Control.Monad.State

import qualified Fe.Abs as A

import Common.Ast
import Common.Types
import Common.Utils
import Common.Annotation
import Common.Scope

import Fe.Abs (Ident)

import TypeCheck.State hiding (Static)
import TypeCheck.Error
import TypeCheck.Variable
import TypeCheck.StateFunctions

getAnnotatedType :: HasAnnotation a => a Annotation -> Type
getAnnotatedType x =
    case getAnnotation x of
        Typed _ t -> t
        Position _ -> TUntyped

repositionAnnotation :: HasAnnotation a => a Annotation -> A.BNFC'Position -> Annotation
repositionAnnotation x p =
    case getAnnotation x of
        Typed _ t -> Typed p t
        Position _ -> Position p

assertType :: Type -> Type -> A.BNFC'Position -> PreprocessorMonad ()
assertType actualType expectedType p = do
    when (expectedType /= actualType) $
        throwError $ TypeMismatch p actualType expectedType

isVariable :: A.CV -> Bool
isVariable (A.Const _) = False
isVariable (A.Var _) = True

isConst :: A.CV -> Bool
isConst (A.Const _) = True
isConst (A.Var _) = False

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
    throwError $ Other "Not yet implemented" p
castLifetime (A.ImplicitLifetime _) = do
    LifetimeState lifetime _ <- gets lifetimeState
    return lifetime

-- getItemIdent :: A.Item' a -> A.Ident
-- getItemIdent (A.ItemFunction _ ident _ _ _) = ident
-- getItemIdent (A.ItemStruct _ ident _) = ident
-- getItemIdent (A.ItemVariant _ ident _) = ident
-- getItemIdent (A.ItemVariable _ _ ident _ _) = ident

getShortestLifetimeOfUsedVariables :: A.BNFC'Position -> PreprocessorMonad Lifetime
getShortestLifetimeOfUsedVariables p = do
    usedVariables <- gets usedVariables
    when (null usedVariables) $ throwError (CannotMakeEmptyReference p)
    lifetimes <- traverse (\id -> do
        variable <- getVariableById id
        return $ lifetime variable) usedVariables
    let head:tail = lifetimes
    foldM (\first second -> do
        if isSubLifetime first second then
            return second
        else if isSubLifetime second first then
            return first
        else
            throwError $ LifetimesMismatch first second
        ) head tail
