module TypeCheck.Utils where

import qualified Fe.Abs as A
import Control.Monad.Except
import Control.Monad.State
import qualified Prelude as C (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Int, Maybe(..))
import Prelude (Integer, String, IO, ($), Traversable (traverse), Maybe (Just), (++), Show (show), Eq (..))
import Common.Ast
import Common.Types
import Data.Map ( empty, insert, fromList, lookup, Map )
import Data.Maybe (isNothing, isJust)
import Fe.Abs (Ident)
import TypeCheck.State
import TypeCheck.Error
import Common.Utils (Identifier)

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

assertTypeOfIdent :: Identifier -> Type -> PreprocessorMonad ()
assertTypeOfIdent ident expectedType = do
    actualType <- getType ident
    when (expectedType /= actualType) $ do
        throwError $ TypeMismatch ident actualType expectedType

modifyType :: A.TypeModifier -> Type -> Type
modifyType (A.None p) t = t
modifyType (A.Ref p lifetime) t = TReference $ Reference Static Const t -- TODO: deal with lifetimes
modifyType (A.MutRef p lifetime) t = TReference $ Reference Static Mutable t -- TODO: deal with lifetimes
