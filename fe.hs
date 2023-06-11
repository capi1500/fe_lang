import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO (hPutStrLn, stderr)
import Control.Monad ( when, unless )
import Control.Monad.State (runState, StateT (runStateT), MonadIO (liftIO))
import Control.Monad.Except (runExceptT, runExcept)
import Data.Either
import Data.List (intercalate)

import qualified Fe.Abs as A
import Fe.Lex   ( Token, mkPosToken )
import Fe.Par   ( pCode, myLexer )
import Fe.Print ( Print, printTree )
import Fe.Skel  ()

import Common.Ast
import Common.Utils
import Common.Printer
import Common.AstPrinter

import Exec.Exec
import Exec.State

import TypeCheck.TypeCheck (typeCheck)
import TypeCheck.Error (PreprocessorError, debug)
import TypeCheck.State (PreprocessorState (PreprocessorState, warnings), makePreprocessorState)
import Compile.Compile (Compile(compile))
import Compile.State

type ParseFun a = [Token] -> Either String a

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        fs -> mapM_ (runFile pCode) fs

usage :: IO ()
usage = do
    printErr $ unlines [
        "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (file)          Execute a file."]

parse :: ParseFun A.Code -> String -> IO ()
parse p s =
    case p ts of
    Left err -> do
        printErr "\nParse              Failed...\n"
        printErr "Tokens:"
        mapM_ (printErr . showPosToken . mkPosToken) ts
        printErr err
        exitFailure
    Right tree -> do
        ast <- typeCheckStage tree
        when debug $ do
            printErr "TypeChecked ast\n"
            printErr $ codePrint 0 ast
        compileStage ast >>= putStr
        executeStage ast
    where
    ts = myLexer s
    showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

type PreprocessorOutput = Code

typeCheckStage :: A.Code -> IO PreprocessorOutput
typeCheckStage code =
    handleTypeCheckError $ runState (runExceptT (typeCheck code)) makePreprocessorState

handleTypeCheckError :: (Either PreprocessorError Code, PreprocessorState) -> IO PreprocessorOutput
handleTypeCheckError (Left err, state) = do
    printErr "Error in type checker"
    printWarnings state
    printErr $ show err
    exitFailure
handleTypeCheckError (Right ast, state) = do
    printWarnings state
    return ast

printWarnings :: PreprocessorState -> IO ()
printWarnings state = do
    let warnings' = warnings state
    unless (null warnings') $ do
        printErr "Warnings:"
        printErr $ intercalate "\n" (fmap (codePrint 0) (reverse warnings'))

compileStage :: PreprocessorOutput -> IO String
compileStage code = do
    handleCompileError $ runState (runExceptT (compile code)) makeCompilationState

handleCompileError :: (Either CompilationError String, CompilationState) -> IO String
handleCompileError (Left err, state) = do
    printErr "Error in compile"
    printErr $ show err
    exitFailure
handleCompileError (Right code, state) = do
    return code

executeStage :: PreprocessorOutput -> IO ()
executeStage code = do
    input <- getContents
    (out, _) <- runStateT (runExceptT (execute code)) (makeExecutionState (words input))
    handleExecutionError out
    return ()

handleExecutionError :: Either ExecutionError () -> IO ()
handleExecutionError (Left err) = do
    printErr "Error during execution"
    printErr $ show err
    exitFailure
handleExecutionError (Right _) = do
    return ()

runFile :: ParseFun A.Code -> FilePath -> IO ()
runFile p f = readFile f >>= parse p

printErr :: String -> IO ()
printErr = hPutStrLn stderr
