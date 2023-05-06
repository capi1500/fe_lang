module TypeCheck.TypesUtils where

import Prelude hiding (lookup)
import Data.Map
import Data.Maybe
import Control.Monad.State

import Common.Utils
import Common.Types
import Common.Scope

import TypeCheck.State
import TypeCheck.Error

getType :: Identifier -> PreprocessorMonad Type
getType ident = do
    p <- gets position
    state <- get
    let maybeType = helper ident (typeDefinitions state)
    when (isNothing maybeType) $ do throw $ TypeNotDefined p ident
    return $ fromJust maybeType
  where
    helper :: Identifier -> TypeDefinitions -> Maybe Type
    helper ident (Global types) = lookup ident types
    helper ident (Local parent types) =
        let x = lookup ident types in
        if isNothing x then helper ident parent
        else x

getTypeLocal :: Identifier -> TypeDefinitions -> Maybe Type
getTypeLocal ident (Global types) = lookup ident types
getTypeLocal ident (Local _ types) = lookup ident types

addType :: Identifier -> Type -> PreprocessorMonad ()
addType ident t = do
    p <- gets position
    state <- get
    let maybeDefinition = getTypeLocal ident (typeDefinitions state)
    when (isJust maybeDefinition) $ do throw $ TypeAlreadyInScope p ident t (fromJust maybeDefinition)
    putTypeDefinitions (helper ident t (typeDefinitions state))
  where
    helper ident t (Global types) = Global (insert ident t types)
    helper ident t (Local parent types) = Local parent (insert ident t types)