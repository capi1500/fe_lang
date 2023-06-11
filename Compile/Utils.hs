module Compile.Utils where

import Common.Types
import Compile.State
import Control.Monad.Except
import Control.Monad.State
import System.IO.Unsafe

unsafePrint :: String -> CompilationMonad ()
unsafePrint str = do
    return $! unsafePerformIO (putStrLn str)

throw :: String -> CompilationMonad a
throw string = throwError $ Other string

typeSize :: Type -> Int
typeSize TUntyped = 0
typeSize (TPrimitive I32) = 8
typeSize (TPrimitive Char) = 1
typeSize (TPrimitive Bool) = 1
typeSize (TPrimitive Unit) = 0
typeSize (TStruct fields) = sum (fmap (\(Field _ t) -> typeSize t) fields)
typeSize (TVariant variants) = 2 + maximum (fmap typeSize variants)
typeSize TFunction {} = 16
typeSize (TArray _) = 16
typeSize (TReference _ _) = 16

compileType :: String -> Type -> CompilationMonad String
compileType _ TUntyped = throw "cannot compile untyped" 
compileType _ (TPrimitive I32) = return "type primitive: i32 4"
compileType _ (TPrimitive Char) = return "type primitive: char 1"
compileType _ (TPrimitive Bool) = return "type primitive: bool 1"
compileType _ (TPrimitive Unit) = throw "cannot compile unit"
compileType ident (TStruct fields) = throw "not implemented" 
compileType ident (TVariant variants) = throw "not implemented"
compileType ident TFunction {} = throw "not implemented"
compileType ident (TArray _) = throw "not implemented"
compileType ident (TReference _ _) = throw "not implemented"
