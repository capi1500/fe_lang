module Common.Scope where

import Data.Map (Map)
import qualified Fe.Abs as A
import Common.Types (Type)

type IdentTypeMap = Map A.Ident Type

data Scope a =
    Global a |
    Local (Scope a) a
    deriving Show
