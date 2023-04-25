module TypeCheck.Utils where

import qualified Fe.Abs as A
import Control.Monad.Except
import Control.Monad.State
import Common.Ast
import Common.Types
import Data.Map ( empty, insert, fromList, lookup, Map )
import Data.Maybe (isNothing, isJust)
import Fe.Abs (Ident)
import TypeCheck.State
import TypeCheck.Error
import Common.Utils (Identifier, Identifier' (Identifier))
import Common.Annotation
import Common.Scope

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

modifyType :: A.TypeModifier -> Type -> Type
modifyType (A.None p) t = t
modifyType (A.Ref p lifetime) t = TReference $ Reference Static Const t -- TODO: deal with lifetimes
modifyType (A.MutRef p lifetime) t = TReference $ Reference Static Mutable t -- TODO: deal with lifetimes

inNewScope :: PreprocessorMonad a -> PreprocessorMonad a
inNewScope f = do
    PreprocessorState scope allocator warnings <- get -- record current state
    let Allocator variableStack freeCells _ = allocator
    let innerScope = Local scope (empty, empty)
    put $ PreprocessorState innerScope allocator warnings
    ret' <- f
    PreprocessorState _ (Allocator _ _ maxSize) warnings' <- get
    put $ PreprocessorState scope (Allocator variableStack freeCells maxSize) warnings' -- reproduce with keeping warnings and updating max stack size
    return ret'

typeOfBlock :: [Statement] -> Type
typeOfBlock statements =
    if null statements then
        TPrimitive Unit
    else
        case last statements of
        ExpressionStatement (TypedExpression _ t) -> t
        _ -> TPrimitive Unit

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
