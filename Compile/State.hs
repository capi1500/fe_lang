module Compile.State where

import Data.Map

import Control.Monad.State
import Control.Monad.Except

import Common.Scope
import Common.Ast hiding (variables)
import Fe.Abs (BNFC'Position)

newtype CompilationError = Other String
  deriving (Eq, Ord, Show, Read)

data FunctionDef = FunctionDef {
    arg_size :: Int,
    local_size :: Int,
    ret_size :: Int,
    code :: [String]
} deriving (Eq, Ord, Show, Read)

data TypeDef = TypeDef {
    size :: Int,
    type_code :: String
} deriving (Eq, Ord, Show, Read)

type Variables = Scope (Map String (Pointer, Int))

data CompilationState = CompilationState {
    types :: Map String TypeDef,
    functions :: Map String FunctionDef,
    variables :: Variables, -- TODO: should introduce block patterns, declared variables not removed
    stack_ptr :: Int,
    expression_stack :: [String], -- TODO: change to stack monad
    current_function :: FunctionDef,
    labels :: [String],
    label_count :: Int
} deriving (Eq, Ord, Show, Read)

type CompilationMonad a = ExceptT CompilationError (State CompilationState) a

data ValueLocation = 
    Imm String Int |
    Stack Pointer Int |
    Empty
  deriving (Eq, Ord, Show, Read)

makeCompilationState :: CompilationState
makeCompilationState = CompilationState {
    types = empty,
    functions = empty,
    variables = Global empty,
    stack_ptr = 0,
    expression_stack = [],
    current_function = FunctionDef 0 0 0 [],
    labels = [],
    label_count = 0
}

