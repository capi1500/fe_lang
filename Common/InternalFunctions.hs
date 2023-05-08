module Common.InternalFunctions where

import Common.Utils
import Common.Types
import Common.Ast
import Exec.StateFunctions (getVariable)
import Control.Monad.Cont (MonadIO(liftIO))
import Common.Printer
import Common.AstPrinter
import Control.Monad.State

makeInternalFunction :: Identifier -> [(Identifier, Type)] -> Type -> Expression -> (Identifier, Type, Value)
makeInternalFunction name params return e =
    let t = TFunction Fn (fmap snd params) return in
    let v = VFunction (fmap fst params) e in
    (name, t, v)

internals :: [(Identifier, Type, Value)]
internals = [
        makeInternalFunction "print_i32" [("v", i32Type)] unitType (InternalExpression printFunction),
        makeInternalFunction "input_i32" [] i32Type (InternalExpression inputI32Function)
    ]

printFunction :: ExecutorMonad Value
printFunction = do
    (_, Variable v) <- getVariable "v"
    liftIO $ putStrLn (codePrint 0 v)
    return VUnit

inputI32Function :: ExecutorMonad Value
inputI32Function = do
    input <- gets input
    let x = read input :: Int
    return $ VI32 x
