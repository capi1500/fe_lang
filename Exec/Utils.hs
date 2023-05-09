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

deref :: Value -> ExecutorMonad Value
deref (VVariable _ (Variable (VReference pointer))) = do
    v <- getVariableById pointer
    return $ VVariable pointer v
deref x = do
    return x

varValue :: Value -> ExecutorMonad Value
varValue (VVariable _ (Variable v)) = return v
varValue x = return x

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

print :: String -> ExecutorMonad ()
print string = liftIO $ putStrLn string
