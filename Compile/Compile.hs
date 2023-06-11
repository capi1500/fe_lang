{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compile.Compile where

import Data.Foldable hiding (toList)
import Data.Map hiding (null)
import Control.Monad.State
import Control.Monad.Except

import Compile.State
import Common.Ast hiding (Other)
import Data.List
import Compile.StateFunctions
import Common.Types
import Compile.Utils

class Compile a b where
    compile :: a -> CompilationMonad b

printFunction :: (String, FunctionDef) -> String
printFunction (name, FunctionDef arg_size local_size ret_size code) = 
    "function " ++ name ++ "{\n" ++ 
    "  arg_size: " ++ show arg_size ++ ";\n" ++
    "  local_size: " ++ show local_size ++ ";\n" ++
    "  ret_size: " ++ show ret_size ++ ";\n\n" ++
    "  code: {\n" ++
    intercalate "" (fmap (\x -> "    " ++ x ++ ";\n") (reverse code)) ++
    "  }\n" ++ 
    "}"

insertInternals :: CompilationMonad ()
insertInternals = do
    addType "i32" i32Type
    addType "char" charType
    addType "bool" boolType

instance Compile Code String where
    compile :: Code -> CompilationMonad String
    compile (Code statements) = do
        insertInternals
        traverse_ initializeGlobalScope statements
        traverse_ (\s -> compile s :: CompilationMonad ValueLocation) statements
        CompilationState types functions _ _ _ _ <- get
        let types' = intercalate "\n" (fmap (\(TypeDef _ str) -> str) (elems types))
        let functions' = intercalate "\n" (fmap printFunction (toList functions))
        return $ types' ++ "\n" ++ functions'

initializeGlobalScope :: Statement -> CompilationMonad ()
initializeGlobalScope (NewFunctionStatement name _ _) = declareFunction name
initializeGlobalScope s = do
    compile s :: CompilationMonad ValueLocation
    return ()

instance Compile Statement ValueLocation where
    compile :: Statement -> CompilationMonad ValueLocation
    compile EmptyStatement = return $ Imm "" 0
    compile (TypeStatement t) = throw "Not yet implemented"
    compile (NewVariableStatement ident initialization) = do
        throw "Not yet implemented"
    compile (NewFunctionStatement ident expression paramNames) = do
        setNewFunction
        compile expression :: CompilationMonad ValueLocation
        function <- gets current_function
        implementFunction ident function
        return $ Imm "" 0
    compile (ExpressionStatement expression) = do
        compile expression :: CompilationMonad ValueLocation
        clearStack
        return $ Imm "" 0

instance Compile Expression ValueLocation where
    compile (BlockExpression statements) = do
        if null statements then do
            return $ Imm "" 0
        else do
            let last:prefix = reverse statements
            traverse_ (\s -> compile s :: CompilationMonad ValueLocation) (reverse prefix)
            compile last
    compile (IfExpression condition onTrue onFalse) = do throw "Not yet implemented"
    compile (WhileExpression cond block) = do throw "Not yet implemented"
    compile (ForExpression var range block) = do throw "Not yet implemented"
    compile (InternalExpression internal) = do throw "Not yet implemented"
    compile (LiteralExpression (VI32 x)) = do
        return $ Imm (show x) 8
    compile (LiteralExpression _) = do throw "Not yet implemented"
    compile (MakeArrayExpression elems) = do throw "Not yet implemented"
    compile (MakeArrayDefaultsExpression size elem) = do throw "Not yet implemented" -- size, default
    compile (MakeClosureExpression captures params block) = do throw "Not yet implemented"
    compile (VariableExpression name) = do
        (pointer, size) <- getVariable name
        return $ Stack pointer size
    compile (ReferenceExpression e) = do throw "Not yet implemented"
    compile (DereferenceExpression e) = do throw "Not yet implemented"
    compile (CallExpression _ f params) = do throw "Not yet implemented"
    compile (IndexExpression _ e1 e2) = do throw "Not yet implemented"
    compile (UnaryMinusExpression e) = do throw "Not yet implemented"
    compile (UnaryNegationExpression e) = do throw "Not yet implemented"
    compile (I32DoubleOperatorExpression _ op e1 e2) = do
        l1 <- compile e1
        l2 <- compile e2
        pointer <- addTemporaryVariable 8 "int32"
        compileFirstArg pointer l1
        let (s1, s3) = compileSecondArg l2
        addOperand $ compileI32DoubleOperator op ++ "_l64_" ++ s1 ++ " " ++ show pointer ++ ", " ++ s3
        return $ Stack pointer 8
    compile (BoolDoubleOperatorExpression op e1 e2) = do throw "Not yet implemented"
    compile (RangeExpression e1 e2) = do throw "Not yet implemented"
    compile (AssignmentExpression place e) = do throw "Not yet implemented" -- expression1, expression2
    compile BreakExpression = do throw "Not yet implemented"
    compile ContinueExpression = do throw "Not yet implemented"
    compile (ReturnExpression e) = do throw "Not yet implemented"

compileI32DoubleOperator :: NumericDoubleOperator -> String
compileI32DoubleOperator Plus = "add"
compileI32DoubleOperator Minus = "sub"
compileI32DoubleOperator Multiply = "mul"
compileI32DoubleOperator Divide = "div"
compileI32DoubleOperator Modulo = "mod"
compileI32DoubleOperator _ = "unknown"  

compileFirstArg :: Pointer -> ValueLocation -> CompilationMonad ()
compileFirstArg tmp (Imm code _) = do
    addOperand $ "mov_l64_imm " ++ show tmp ++ ", " ++ code
compileFirstArg tmp (Stack ptr _) = do
    addOperand $ "mov_l64_l64 " ++ show tmp ++ ", " ++ show ptr

compileSecondArg :: ValueLocation -> (String, String)
compileSecondArg (Imm code _) = ("imm", code)
compileSecondArg (Stack ptr _) = ("l64", show ptr)
