module Compile.StateFunctions where

import Control.Monad.State

import Common.Utils
import Compile.State
import Common.Types
import Compile.Utils
import Data.Map hiding (null)
import Common.Scope
import Prelude hiding (lookup)
import Common.Ast hiding (variables)
import Data.Maybe
import TypeCheck.VariablesUtils (addTemporaryVariable)
import Language.Haskell.TH (unsafe)

addType :: Identifier -> Type -> CompilationMonad ()
addType ident t = do
    CompilationState types functions variables stack_ptr expression_stack current_code labels label_count <- get
    t' <- compileType ident t
    put $ CompilationState (insert ident (TypeDef (typeSize t) t') types) functions variables stack_ptr expression_stack current_code labels label_count

declareFunction :: Identifier -> CompilationMonad ()
declareFunction ident = do
    CompilationState types functions variables stack_ptr expression_stack current_code labels label_count <- get
    put $ CompilationState types (insert ident (FunctionDef 0 0 0 []) functions) variables stack_ptr expression_stack current_code labels label_count

implementFunction :: Identifier -> FunctionDef -> CompilationMonad ()
implementFunction ident def = do
    CompilationState types functions variables stack_ptr expression_stack current_code labels label_count <- get
    put $ CompilationState types (insert ident def functions) variables stack_ptr expression_stack current_code labels label_count

initVariable :: Identifier -> Int -> String -> CompilationMonad Pointer
initVariable ident size t = do
    CompilationState types functions variables stack_ptr expression_stack current_code labels label_count <- get
    put $ CompilationState types functions (helper variables (stack_ptr, size)) stack_ptr expression_stack current_code labels label_count
    pushStack size
    addOperand $ "init_type " ++ t
    return stack_ptr
  where
    helper (Global map) ptr = Global (insert ident ptr map)
    helper (Local parent map) ptr = Local parent (insert ident ptr map)

-- can be safely called only when ident is on top of the stack
deinitVariable :: CompilationMonad ()
deinitVariable = do
    CompilationState types functions variables stack_ptr expression_stack current_code labels label_count <- get
    if null expression_stack then do
        throw "Cannot deinit empty stack"
    else do
        let ident:expression_stack' = expression_stack
        let (variable', Just (_, size)) = helper variables ident
        put $ CompilationState types functions variable' stack_ptr expression_stack' current_code labels label_count
        pushStack (-size)
        addOperand "deinit"
  where
    helper (Global map) ident = (Global (delete ident map), lookup ident map)
    helper (Local parent map) ident = (Local parent (delete ident map), lookup ident map)

addTemporaryVariable :: Int -> String -> CompilationMonad Pointer
addTemporaryVariable size t = do 
    CompilationState types functions variables stack_ptr expression_stack current_code labels label_count <- get
    x <- initVariable ("tmp_" ++ show stack_ptr) size t
    CompilationState types functions variables stack_ptr expression_stack current_code labels label_count <- get
    put $ CompilationState types functions variables stack_ptr (("tmp_" ++ show stack_ptr):expression_stack) current_code labels label_count
    return x

getVariable :: Identifier -> CompilationMonad (Pointer, Int)
getVariable ident = do
    variables <- gets variables
    return $ fromJust (helper variables)
  where
    helper (Global map) = lookup ident map
    helper (Local parent map) = lookup ident map

clearStack :: CompilationMonad ()
clearStack = do
    CompilationState types functions variables stack_ptr expression_stack function_def labels label_count <- get
    clear expression_stack
    current_code <- getCurrentCode
    put $ CompilationState types functions variables stack_ptr [] function_def labels label_count
    setCurrentCode current_code
  where
    clear [] = return ()
    clear (_:xs) = do
        deinitVariable
        clear xs
 
addOperand :: String -> CompilationMonad ()
addOperand operand = do
    CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c current_code) labels label_count <- get
    put $ CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c (operand:current_code)) labels label_count

pushStack :: Int -> CompilationMonad ()
pushStack size = do
    CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c current_code) labels label_count <- get
    put $ CompilationState types functions variables (stack_ptr + size) expression_stack (FunctionDef a (max b (stack_ptr + size)) c current_code) labels label_count

setNewFunction :: CompilationMonad ()
setNewFunction = do
    CompilationState types functions variables _ expression_stack _ labels label_count <- get
    put $ CompilationState types functions (Global empty) 0 expression_stack (FunctionDef 0 0 0 []) labels label_count
  
getCurrentCode :: CompilationMonad [String]
getCurrentCode = do
    FunctionDef _ _ _ current_code <- gets current_function
    return current_code

setCurrentCode :: [String] -> CompilationMonad ()
setCurrentCode current_code = do
    CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c _) labels label_count <- get
    put $ CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c current_code) labels label_count

newLabel :: CompilationMonad Int
newLabel = do
    CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c current_code) labels label_count <- get
    put $ CompilationState types functions variables stack_ptr expression_stack (FunctionDef a b c current_code) labels (label_count + 1)
    return label_count 

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
