module TypeCheck.LifetimeUtils where


import Data.Maybe
import Data.List
import Control.Monad.State

import Fe.Abs (BNFC'Position)

import Common.Utils

import TypeCheck.State
import TypeCheck.Variable

getLifetime :: PreprocessorMonad Lifetime
getLifetime = do
    LifetimeState lifetime _ <- gets lifetimeState
    return lifetime

saveLifetime :: Lifetime -> PreprocessorMonad ()
saveLifetime lifetime = do
    LifetimeState _ id <- gets lifetimeState
    putLifetimeState $ LifetimeState lifetime id

-- first is subLifetime of second if and only if first lives longer or equal compared to second. (therefore second lifetime can be used in place of the first one)
isSubLifetime :: Lifetime -> Lifetime -> Bool
isSubLifetime (Lifetime first _) (Lifetime second _) = first `isPrefixOf` second

-- updates lifetime state, but keeps current lifetime saved value
forkLifetime :: BNFC'Position -> Lifetime -> PreprocessorMonad Lifetime
forkLifetime p (Lifetime list _) = do
    let line = if isJust p then
            let (l, _) = fromJust p in l
        else
            -1
    LifetimeState savedLifetime id <- gets lifetimeState
    let lifetime' = Lifetime (listPushBack id list) line
    putLifetimeState $ LifetimeState savedLifetime (id + 1)
    return lifetime'

-- updates lifetime state and changes saved lifetime
updateLifetime :: BNFC'Position -> PreprocessorMonad ()
updateLifetime p = do
    lifetime <- getLifetime
    lifetime' <- forkLifetime p lifetime
    saveLifetime lifetime'