module TypeCheck.Printer where

import Data.List (intercalate)
import Data.Maybe
import Control.Monad.State

import Common.Types
import Common.Printer
import Common.AstPrinter

import TypeCheck.State
import TypeCheck.Error
import TypeCheck.Variable


printDebug str = do
    when debug $ addWarning $ Debug str

printVariables :: PreprocessorMonad ()
printVariables = do
    getPrintVariables >>= printDebug

getPrintVariables :: PreprocessorMonad String
getPrintVariables = do
    Variables _ variables <- gets variables
    return $ "Variables: [\n" ++
        intercalate ",\n"
            (fmap
                (\v -> printTabs 1 ++ codePrint 2 v)
                variables)
        ++ "]"

instance CodePrint ExpressionType where
    codePrint tabs (PlaceType mut id) = show mut ++ " pointer " ++ show id
    codePrint tabs (ValueType value) = codePrint tabs value

instance CodePrint TypedExpression where
    codePrint tabs (TypedExpression expr _) = codePrint tabs expr

instance CodePrint Variables where
  codePrint tabs (Variables _ list) = "[" ++ intercalate ("\n" ++ printTabs tabs) (fmap show list) ++ "]"

instance CodePrint Value where
    codePrint tabs (Value t _ borrows borrowsMut owned) =
        "Value {\n" ++
        printTabs (tabs + 1) ++ codePrint tabs t ++ "\n" ++
        printTabs (tabs + 1) ++ "borrows: " ++ intercalate ", " (fmap (\(i, w) -> "(" ++ show i ++ ", " ++ show (fromJust w) ++ ")") borrows) ++ "\n" ++
        printTabs (tabs + 1) ++ "borrowsMut: " ++ intercalate ", " (fmap (\(i, w) -> "(" ++ show i ++ ", " ++ show (fromJust w) ++ ")") borrowsMut) ++ "\n" ++
        printTabs (tabs + 1) ++ "owned: " ++ show owned ++ "\n" ++
        printTabs tabs ++ "}"

instance CodePrint Variable where
    codePrint tabs (Variable createdAt maybeIdent t id mutability state value lifetime) =
        let name = fromMaybe "temporary" maybeIdent in
        let Lifetime list _ = lifetime in
        "Variable " ++ show id ++ " {\n" ++
        printTabs (tabs + 1) ++ show name ++ ": " ++ (if isConst mutability then "const " else "") ++ codePrint tabs t ++ "\n" ++
        (if state /= Moved then
        printTabs (tabs + 1) ++ codePrint (tabs + 1) value ++ "\n" ++
        printTabs (tabs + 1) ++ show state ++ "\n" ++
        printTabs (tabs + 1) ++ "lifetime: " ++ codePrint tabs list ++ "\n"
        else "") ++
        printTabs (tabs + 1) ++ "created at: " ++ show createdAt ++ "\n" ++
        printTabs tabs ++ "}"
