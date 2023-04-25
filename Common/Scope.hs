module Common.Scope where

data Scope a =
    Global a |
    Local (Scope a) a
  deriving (Eq, Ord, Show, Read)

isGlobal :: Scope a -> Bool
isGlobal (Global _) = True
isGlobal (Local _ _) = False

isLocal :: Scope a -> Bool
isLocal (Global _) = False
isLocal (Local _ _) = True
