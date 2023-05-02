module Common.Utils where

import qualified Fe.Abs as A
import Data.Map (Map, lookup, empty)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isNothing)
import Common.Printer

type Identifier = Identifier' A.BNFC'Position
data Identifier' a = Identifier a A.Ident
  deriving (Eq, Ord, Show, Read)

instance CodePrint (Identifier' a) where
    codePrint _ (Identifier _ (A.Ident ident)) = ident

listSet :: Int -> a -> [a] -> [a]
listSet id value list =
    let (pref, _:suf) = splitAt id list in
    (pref++(value:suf))

listPushBack :: a -> [a] -> [a]
listPushBack value list = list ++ [value]

listGet :: Int -> [a] -> a
listGet id list = list !! id
