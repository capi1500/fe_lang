module Exec.Utils where
import Common.Ast
import Common.Types
import Exec.State
import Exec.StateFunctions (getVariableById)
import Common.Scope
import Control.Monad.State
import Data.Map (toList, Map)
import Fe.Abs (Ident(..))
import Common.Printer
import Common.AstPrinter
import Data.List (intercalate, sortBy)
import Common.Utils

valueOfBlock :: [Value] -> Value
valueOfBlock statements =
    if null statements then
        VUnit
    else
        last statements

-- derefConditionally :: Bool -> ExecutorMonad Value -> ExecutorMonad Value
-- derefConditionally shouldBeReference exec = do
--     exec >>= if shouldBeReference then do
--             return
--         else do
--             deref

deref :: Value -> ExecutorMonad Value
deref (VReference pointer) = do
    Variable value <- getVariableById pointer
    return value
deref x = do
    return x

-- derefVariable :: Bool -> Variable -> ExecutorMonad Variable
-- derefVariable shouldBeReference Uninitialized = do
--     return Uninitialized
-- derefVariable shouldBeReference (Variable value) = do
--     v' <- if shouldBeReference then do
--         return value
--     else do
--         deref value
--     return $ Variable v'

instance CodePrint Variable where
    codePrint tabs Uninitialized = "null"
    codePrint tabs (Variable v) = codePrint tabs v

printLocalScope :: ExecutorMonad ()
printLocalScope = do
    mappings <- gets variableMappings
    printScope (helper mappings)
  where
    helper (Global map) = map
    helper (Local _ map) = map

printScope :: Map Identifier Pointer -> ExecutorMonad ()
printScope map = do
    vars <- traverse (\(ident, id) -> do
        x <- getVariableById id
        return $ "    " ++ show id ++ ": " ++ ident ++ " = " ++ codePrint 1 x ++ "\n")
        (sortBy (\(_, id1) (_, id2) -> compare id1 id2) (toList map))
    liftIO $ putStrLn (intercalate "" vars)

printVariables :: ExecutorMonad ()
printVariables = do
    mappings <- gets variableMappings
    helper mappings
  where
    helper (Global map) = do
        printScope map
    helper (Local parent map) = do
        helper parent
        printScope map
