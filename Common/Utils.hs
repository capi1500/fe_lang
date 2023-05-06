module Common.Utils where

import qualified Fe.Abs as A
import Data.Map (Map, lookup, empty)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isNothing)
import Common.Printer

type Identifier = String

listSet :: Int -> a -> [a] -> [a]
listSet id value list =
    let (pref, _:suf) = splitAt id list in
    (pref++(value:suf))

listPushBack :: a -> [a] -> [a]
listPushBack value list = list ++ [value]

listGet :: Int -> [a] -> a
listGet id list = list !! id
