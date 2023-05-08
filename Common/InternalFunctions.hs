module Common.InternalFunctions where

import Data.Maybe
import Text.Read
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Except

import Common.Utils
import Common.Types
import Common.Ast
import Common.Printer
import Common.AstPrinter

import Exec.StateFunctions
import Exec.State
import Exec.Error
import Data.ByteString (intercalate)

makeInternalFunction :: Identifier -> [(Identifier, Type)] -> Type -> Expression -> (Identifier, Type, Value)
makeInternalFunction name params return e =
    let t = TFunction Fn (fmap snd params) return in
    let v = VFunction (fmap fst params) e in
    (name, t, v)

internals :: [(Identifier, Type, Value)]
internals = [
        makeInternalFunction "print_i32" [("v", i32Type)] unitType (InternalExpression printFunction),
        makeInternalFunction "print_char" [("v", charType)] unitType (InternalExpression printCharFunction),
        makeInternalFunction "print_bool" [("v", boolType)] unitType (InternalExpression printFunction),
        makeInternalFunction "print_string" [("v", stringType)] unitType (InternalExpression printStringFunction),
        makeInternalFunction "input_i32" [] i32Type (InternalExpression inputI32Function),
        makeInternalFunction "input_char" [] charType (InternalExpression inputCharFunction),
        makeInternalFunction "input_bool" [] boolType (InternalExpression inputBoolFunction),
        makeInternalFunction "input_string" [] stringType (InternalExpression inputStringFunction)
    ]

printFunction :: ExecutorMonad Value
printFunction = do
    (_, Variable v) <- getVariable "v"
    liftIO $ putStr (codePrint 0 v)
    return VUnit

printCharFunction :: ExecutorMonad Value
printCharFunction = do
    (_, Variable (VChar c)) <- getVariable "v"
    liftIO $ putStr [c]
    return VUnit


printStringFunction :: ExecutorMonad Value
printStringFunction = do
    (_, Variable (VArray lst)) <- getVariable "v"
    variables <- traverse getVariableById lst
    let chars = fmap (\(Variable (VChar c)) -> c) variables
    liftIO $ putStr chars
    return VUnit

inputI32Function :: ExecutorMonad Value
inputI32Function = do
    input <- gets input
    let word = head input
    putInput $ tail input
    let x = readMaybe word :: Maybe Int
    when (isNothing x) $ throwError InputFailed
    return $ VI32 (fromJust x)

inputBoolFunction :: ExecutorMonad Value
inputBoolFunction = do
    input <- gets input
    let word = head input
    putInput $ tail input
    if word == "true" then do
        return $ VBool True
    else if word == "false" then do
        return $ VBool False
    else
        throwError InputFailed

inputStringFunction :: ExecutorMonad Value
inputStringFunction = do
    input <- gets input
    let word = head input
    putInput $ tail input
    pointers <- traverse (\v -> do addTmpVariable (Variable (VChar v))) word
    return $ VArray pointers

inputCharFunction :: ExecutorMonad Value
inputCharFunction = do
    input <- gets input
    let word = head input
    let rest = tail input
    c <- if length word == 1 then do
            putInput rest
            return $ listGet 0 word
        else do
            let h:t = word
            putInput $ t:rest
            return h
    return $ VChar c

-- lengthFunction :: ExecutorMonad Value
-- lengthFunction = do
--     (_, Variable (VReference ptr)) <- getVariable "v"
--     Variable (VArray lst) <- getVariableById ptr
--     return $ VI32 (length lst)
