module Common.Utils where

import qualified Fe.Abs as A
import Common.Ast
import Common.Types
import Data.Map (Map, lookup, empty)
import Prelude (String, Maybe (Just), (++), Show (show), Eq (..), ($))
import qualified Prelude as C (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Int, Maybe(..))
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isNothing)
import Common.Scope (IdentTypeMap, Scope)

getItemIdent :: A.Item' a -> A.Ident
getItemIdent (A.ItemFunction _ (A.Function _ ident _ _ _)) = ident
getItemIdent (A.ItemStruct _ (A.Struct _ ident _)) = ident
getItemIdent (A.ItemVariant _ (A.Variant _ ident _)) = ident
getItemIdent (A.ItemConst _ (A.Const _ (A.CVDeclaration _ ident _ _))) = ident
getItemIdent (A.ItemVariable _ (A.Variable _ (A.CVDeclaration _ ident _ _))) = ident
