module TypeCheck.State where

import Data.Map
import Control.Monad.State
import Control.Monad.Except

import Fe.Abs (BNFC'Position, Ident (Ident))

import Common.Utils
import Common.Scope
import Common.Types

import TypeCheck.Error
import TypeCheck.Variable
import Data.List (intercalate)
import Common.Printer

type TypeDefinitions = Scope (Map Ident Type)
type VariableMappings = Scope (Map Ident VariableId)
data Variables = Variables VariableMappings [Variable]
    deriving (Eq, Ord, Show, Read)
data LifetimeState = LifetimeState Lifetime Int
    deriving (Eq, Ord, Show, Read)

data Context = None | LValue | RValue
    deriving (Eq, Ord, Show, Read)

data PreprocessorState = PreprocessorState {
    typeDefinitions :: TypeDefinitions, -- follows code scopes
    variables :: Variables, -- follows code scopes, resets on function frames
    lifetimeState :: LifetimeState, -- current lifetime
    warnings :: [PreprocessorWarning], -- only grows
    usedVariables :: Maybe VariableId,
    context :: Context
} deriving (Eq, Ord, Show, Read)

type PreprocessorMonad a = StateT PreprocessorState (Except (PreprocessorError, PreprocessorState)) a

makePreprocessorState :: PreprocessorState
makePreprocessorState = PreprocessorState {
    typeDefinitions = Global (
        fromList [
            (Ident "i32", TPrimitive I32),
            (Ident "char", TPrimitive Char),
            (Ident "bool", TPrimitive Bool),
            (Ident "()", TPrimitive Unit),
            (Ident "String", TArray charType)]
    ),
    variables = Variables (Global empty) [],
    lifetimeState = LifetimeState staticLifetime 1,
    warnings = [],
    usedVariables = Nothing,
    context = None
}

staticLifetime = Lifetime [0] 1

putTypeDefinitions :: TypeDefinitions -> PreprocessorMonad ()
putTypeDefinitions typeDefinitions = do
    PreprocessorState _ variables currentLifetime warnings usedVariables context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings usedVariables context

putVariables :: Variables -> PreprocessorMonad ()
putVariables variables = do
    PreprocessorState typeDefinitions _ currentLifetime warnings usedVariables context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings usedVariables context

putLifetimeState :: LifetimeState -> PreprocessorMonad ()
putLifetimeState lifetimeState = do
    PreprocessorState typeDefinitions variables _ warnings usedVariables context <- get
    put $ PreprocessorState typeDefinitions variables lifetimeState warnings usedVariables context

putWarnings :: [PreprocessorWarning] -> PreprocessorMonad ()
putWarnings warnings = do
    PreprocessorState typeDefinitions variables currentLifetime _ usedVariables context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings usedVariables context

clearUsedVariables :: PreprocessorMonad ()
clearUsedVariables = do
    PreprocessorState typeDefinitions variables currentLifetime warnings _ context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings Nothing context

addWarning :: PreprocessorWarning -> PreprocessorMonad ()
addWarning warning = do
    PreprocessorState typeDefinitions variables currentLifetime warnings usedVariables context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime (warning:warnings) usedVariables context

putContext :: Context -> PreprocessorMonad ()
putContext context = do
    PreprocessorState typeDefinitions variables currentLifetime warnings usedVariables _ <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings usedVariables context

instance CodePrint Variables where
  codePrint tabs (Variables _ list) = "[" ++ intercalate ("\n" ++ printTabs tabs) (fmap show list) ++ "]"

throw :: PreprocessorError -> PreprocessorMonad a
throw err = do
    state <- get
    throwError (err, state)
