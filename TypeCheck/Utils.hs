module TypeCheck.Utils where

import Common.Scope
import qualified Fe.Abs as A
import Control.Monad.Except
import Control.Monad.State
import qualified Prelude as C (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Int, Maybe(..))
import Prelude (Integer, String, IO, ($), Traversable (traverse), Maybe (Just), (++), Show (show), Eq (..))
import Common.Ast
import Common.Types
import Data.Map ( empty, insert, fromList, lookup )
import Data.Maybe (isNothing, isJust)

type TypeCheckState = Scope IdentTypeMap
data TypeCheckError = TypeCheckError String A.BNFC'Position
    deriving C.Show

type TypeCheckM a = StateT TypeCheckState (Except TypeCheckError) a


makeEmptyTypeCheckState :: TypeCheckState
makeEmptyTypeCheckState = Global $ fromList [
            (A.Ident "i32", TPrimitive I32),
            (A.Ident "char", TPrimitive Char),
            (A.Ident "bool", TPrimitive Bool),
            (A.Ident "()", TPrimitive Unit)]


getFromCurrentScope :: A.Ident -> TypeCheckState -> Maybe Type
getFromCurrentScope ident (Global map) = lookup ident map
getFromCurrentScope ident (Local _ map) = lookup ident map

getFromScope :: A.Ident -> TypeCheckState -> Maybe Type
getFromScope ident (Global map) = lookup ident map
getFromScope ident (Local parent map) =
    let x = lookup ident map in
    if isNothing x then getFromScope ident parent
    else x

addToScope :: A.Ident -> Type -> TypeCheckState -> TypeCheckState
addToScope ident t (Global map) = Global $ insert ident t map
addToScope ident t (Local parent map) = Local parent (insert ident t map)

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

tryAddIdentToScope :: A.Ident -> Type -> A.BNFC'Position -> TypeCheckM ()
tryAddIdentToScope ident t p = do
    scope <- get
    if isJust $ getFromCurrentScope ident scope then do
        throwError $ TypeCheckError ("Ident `" ++ show ident ++ "` is in current scope") p
    else do
        put $ addToScope ident t scope
        return ()

getTypeOfIdent :: A.Ident -> A.BNFC'Position -> TypeCheckM Type
getTypeOfIdent ident p = do
    scope <- get
    let maybeT = getFromScope ident scope
    if isNothing maybeT then do
        let A.Ident name = ident
        throwError $ TypeCheckError ("Identifier `" ++ name ++ "` not in scope") p
    else do
        let Just t = maybeT
        return t

assertTypeOfIdent :: A.Ident -> Type -> A.BNFC'Position -> TypeCheckM ()
assertTypeOfIdent ident expectedType p = do
    actualType <- getTypeOfIdent ident p
    let A.Ident name = ident
    when (expectedType /= actualType) $ do
        throwError $ TypeCheckError ("Identifier `" ++ name ++ "` has type `" ++ show actualType ++ "` but type `" ++ show expectedType ++ "` was expected") p

modifyType :: A.TypeModifier -> Type -> Type
modifyType (A.None p) t = t
modifyType (A.Ref p) t = t -- TODO: deal with lifetimes
modifyType (A.RefLifetime p lifetime) t = t -- TODO: deal with lifetimes
modifyType (A.MutRef p) t = t -- TODO: deal with lifetimes
modifyType (A.MutRefLifetime p lifetime) t = t -- TODO: deal with lifetimes
