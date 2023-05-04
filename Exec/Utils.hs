module Exec.Utils where
import Common.Ast
import Common.Types
import Exec.State
import Exec.StateFunctions (getVariableById)
import Common.Scope
import Control.Monad.State
import Data.Map (toList)
import Fe.Abs (Ident(..))

valueOfBlock :: [Value] -> Value
valueOfBlock statements =
    if null statements then
        VUnit
    else
        last statements

derefConditionally :: Bool -> ExecutorMonad Value -> ExecutorMonad Value
derefConditionally isRef exec = do
    exec >>= if isRef then do
            return
        else do
            deref

deref :: Value -> ExecutorMonad Value
deref (VReference pointer) = do
    Variable value <- getVariableById pointer
    return value
deref x = do
    return x

derefVariable :: Bool -> Variable -> ExecutorMonad Variable
derefVariable isRef Uninitialized = do
    return Uninitialized
derefVariable isRef (Variable value) = do
    v' <- if isRef then do
        deref value
    else do
        return value
    return $ Variable v'

printLocalScope :: ExecutorMonad ()
printLocalScope = do
    mappings <- gets variableMappings
    vars <- traverse (\(Ident ident, id) -> do
        x <- getVariableById id
        return (ident, id, x)) (toList (helper mappings))
    liftIO $ print vars
  where
    helper (Global map) = map
    helper (Local _ map) = map
