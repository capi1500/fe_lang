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

printLocalScope :: ExecutorMonad ()
printLocalScope = do
    mappings <- gets variableMappings
    vars <- traverse (\(ident, id) -> do
        x <- getVariableById id
        return (ident, id, x)) (toList (helper mappings))
    liftIO $ print vars
  where
    helper (Global map) = map
    helper (Local _ map) = map
