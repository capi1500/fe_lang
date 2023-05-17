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
import Data.ByteString (intercalate)
import Exec.Utils

makeInternalFunction :: Identifier -> [(Identifier, Type)] -> Type -> Expression -> (Identifier, Type, Value)
makeInternalFunction name params return e =
    let t = TFunction Fn [] (fmap snd params) return in
    let v = VFunction [] (fmap fst params) e in
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
        makeInternalFunction "input_string" [] stringType (InternalExpression inputStringFunction),
        makeInternalFunction "length_i32" [("v", TReference Const (TArray i32Type))] i32Type (InternalExpression lengthFunction),
        makeInternalFunction "length_char" [("v", TReference Const (TArray charType))] i32Type (InternalExpression lengthFunction),
        makeInternalFunction "length_bool" [("v", TReference Const (TArray boolType))] i32Type (InternalExpression lengthFunction),
        makeInternalFunction "length_string" [("v", TReference Const stringType)] i32Type (InternalExpression lengthFunction),
        makeInternalFunction "length_array_i32" [("v", TReference Const (TArray (TArray i32Type)))] i32Type (InternalExpression lengthFunction),
        makeInternalFunction "iter_i32" [("v", TReference Const (TArray i32Type))] (TArray (TReference Const i32Type)) (InternalExpression iterFunction),
        makeInternalFunction "iter_bool" [("v", TReference Const (TArray boolType))] (TArray (TReference Const boolType)) (InternalExpression iterFunction),
        makeInternalFunction "iter_char" [("v", TReference Const (TArray charType))] (TArray (TReference Const charType)) (InternalExpression iterFunction),
        makeInternalFunction "iter_string" [("v", TReference Const stringType)] (TArray (TReference Const charType)) (InternalExpression iterFunction),
        makeInternalFunction "iter_array_i32" [("v", TReference Const (TArray (TArray i32Type)))] (TArray (TReference Const (TArray i32Type))) (InternalExpression iterFunction)
    ]

printFunction :: ExecutorMonad Value
printFunction = do
    (_, Variable v) <- getVariable "v"
    liftIO $ putStr (codePrint 0 v)
    pack VUnit

printCharFunction :: ExecutorMonad Value
printCharFunction = do
    (_, Variable (VChar c)) <- getVariable "v"
    liftIO $ putStr [c]
    pack VUnit

printStringFunction :: ExecutorMonad Value
printStringFunction = do
    (_, Variable (VArray lst)) <- getVariable "v"
    variables <- traverse getVariableById lst
    let chars = fmap (\(Variable (VChar c)) -> c) variables
    liftIO $ putStr chars
    pack VUnit

inputI32Function :: ExecutorMonad Value
inputI32Function = do
    p <- gets position
    input <- gets input
    let word = head input
    putInput $ tail input
    let x = readMaybe word :: Maybe Int
    when (isNothing x) $ throwError (InputFailed p)
    pack $ VI32 (fromJust x)

inputBoolFunction :: ExecutorMonad Value
inputBoolFunction = do
    input <- gets input
    let word = head input
    putInput $ tail input
    if word == "true" then do
        pack $ VBool True
    else if word == "false" then do
        pack $ VBool False
    else do
        p <- gets position
        throwError (InputFailed p)

inputStringFunction :: ExecutorMonad Value
inputStringFunction = do
    input <- gets input
    let word = head input
    putInput $ tail input
    pointers <- traverse (\v -> do addTmpVariable (Variable (VChar v))) word
    pack $ VArray pointers

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
    pack $ VChar c

lengthFunction :: ExecutorMonad Value
lengthFunction = do
    (_, Variable (VReference ptr)) <- getVariable "v"
    Variable (VArray lst) <- getVariableById ptr
    pack $ VI32 (length lst)

iterFunction :: ExecutorMonad Value
iterFunction = do
    (_, Variable (VReference ptr)) <- getVariable "v"
    Variable (VArray lst) <- getVariableById ptr
    pointers <- traverse (\ptr -> do addTmpVariable (Variable (VReference ptr))) lst
    pack $ VArray pointers
