module Compile.StateFunctions where

import Control.Monad.State

import Common.Utils
import Compile.State
import Common.Types
import Compile.Utils
import Data.Map
import Common.Scope
import Prelude hiding (lookup)
import Common.Ast hiding (variables)
import Data.Maybe
import TypeCheck.VariablesUtils (addTemporaryVariable)
import Language.Haskell.TH (unsafe)

addType :: Identifier -> Type -> CompilationMonad ()
addType ident t = do
    CompilationState types functions variables stack_ptr expression_stack current_code <- get
    t' <- compileType ident t
    put $ CompilationState (insert ident (TypeDef (typeSize t) t') types) functions variables stack_ptr expression_stack current_code

declareFunction :: Identifier -> CompilationMonad ()
declareFunction ident = do
    CompilationState types functions variables stack_ptr expression_stack current_code <- get
    put $ CompilationState types (insert ident (FunctionDef 0 0 0 []) functions) variables stack_ptr expression_stack current_code

implementFunction :: Identifier -> FunctionDef -> CompilationMonad ()
implementFunction ident def = do
    CompilationState types functions variables stack_ptr expression_stack current_code <- get
    put $ CompilationState types (insert ident def functions) variables stack_ptr expression_stack current_code

initVariable :: Identifier -> Int -> String -> CompilationMonad ()
initVariable ident size t = do
    CompilationState types functions variables stack_ptr expression_stack current_code <- get
    put $ CompilationState types functions (helper variables (stack_ptr, size)) stack_ptr expression_stack current_code
    pushStack size
    addOperand $ "init_type " ++ t
  where
    helper (Global map) ptr = Global (insert ident ptr map)
    helper (Local parent map) ptr = Local parent (insert ident ptr map)

-- can be safely called only when ident is on top of the stack
deinitVariable :: Identifier -> CompilationMonad ()
deinitVariable ident = do
    CompilationState types functions variables stack_ptr expression_stack current_code <- get
    let (variable', Just (_, size)) = helper variables
    put $ CompilationState types functions variable' stack_ptr expression_stack current_code
    pushStack (-size)
    addOperand "deinit"
  where
    helper (Global map) = (Global (delete ident map), lookup ident map)
    helper (Local parent map) = (Local parent (delete ident map), lookup ident map)

addTemporaryVariable :: Int -> String -> CompilationMonad Pointer
addTemporaryVariable size t = do 
    CompilationState types functions variables stack_ptr expression_stack current_code <- get
    initVariable ("tmp_" ++ show stack_ptr) size t
    return stack_ptr

getVariable :: Identifier -> CompilationMonad (Pointer, Int)
getVariable ident = do
    variables <- gets variables
    return $ fromJust (helper variables)
  where
    helper (Global map) = lookup ident map
    helper (Local parent map) = lookup ident map

clearStack :: CompilationMonad ()
clearStack = do
    gets expression_stack >>= clear
  where
    clear [] = return ()
    clear (x:xs) = do
        unsafePrint $ show x
        deinitVariable x
        clear xs
 
addOperand :: String -> CompilationMonad ()
addOperand operand = do
    CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c current_code) <- get
    put $ CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c (operand:current_code))

pushStack :: Int -> CompilationMonad ()
pushStack size = do
    CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c current_code) <- get
    put $ CompilationState types functions variables (stack_ptr + size) expression_stack (FunctionDef a (max b (stack_ptr + size)) c current_code)

setNewFunction :: CompilationMonad ()
setNewFunction = do
    CompilationState types functions variables _ expression_stack _ <- get
    put $ CompilationState types functions (Global empty) 0 expression_stack (FunctionDef 0 0 0 [])
  
-- addVariable :: Identifier -> Variable -> ExecutorMonad Pointer
-- addVariable ident variable = do
--     ExecutionState mappings variables input p <- get
--     let id = length variables
--     put $ ExecutionState (helper mappings id) (listPushBack variable variables) input p
--     return id
--   where
--     helper (Global map) id = Global $ insert ident id map
--     helper (Local parent map) id = Local parent (insert ident id map)

-- addTmpVariable :: Variable -> ExecutorMonad Pointer
-- addTmpVariable variable = do
--     variables <- gets variables
--     let id = length variables
--     putVariables $ listPushBack variable variables
--     return id

-- getVariable :: Identifier -> ExecutorMonad (Pointer, Variable)
-- getVariable ident = do
--     ExecutionState mappings variables _ _ <- get
--     let id = helper mappings ident
--     return (id, listGet id variables)
--   where
--     helper :: VariableMappings -> Identifier -> Pointer
--     helper (Global map) ident = fromJust (lookup ident map)
--     helper (Local parent map) ident =
--         let maybeId = lookup ident map in
--         fromMaybe (helper parent ident) maybeId

-- getVariableById :: Pointer -> ExecutorMonad Variable
-- getVariableById id = do
--     variables <- gets variables
--     return $ listGet id variables

-- inNewScope :: ExecutorMonad a -> ExecutorMonad a
-- inNewScope f = do
--     state <- get
--     let mappings = variableMappings state  -- record current state
--     putMappings $ Local mappings empty
--     ret' <- f
--     putMappings mappings
--     return ret'

-- makeNewFrame :: Variables -> Variables
-- makeNewFrame (Global global) = Local (Global global) empty
-- makeNewFrame (Local parent _) = makeNewFrame parent
