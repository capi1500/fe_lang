import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when, return )
import Control.Monad.State (runState, StateT (runStateT), MonadIO (liftIO))
import Control.Monad.Except (runExceptT, runExcept)
import Data.Either

import qualified Fe.Abs as A
import Fe.Lex   ( Token, mkPosToken )
import Fe.Par   ( pCode, myLexer )
import Fe.Print ( Print, printTree )
import Fe.Skel  ()

import Common.Annotation
import Common.Ast
import Common.Utils

import TypeCheck.TypeCheck (typeCheck)
import TypeCheck.Error (PreprocessorError)
import TypeCheck.State (PreprocessorState (PreprocessorState), makePreprocessorState, Allocator (Allocator))

import Exec.Exec
import Exec.State
import Exec.Error

type ParseFun a = [Token] -> Either String a

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        fs -> mapM_ (runFile pCode) fs

usage :: IO ()
usage = do
    putStrLn $ unlines [
        "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (file)          Parse content of files verbosely."]

parse :: ParseFun A.Code -> String -> IO ()
parse p s =
    case p ts of
    Left err -> do
        putStrLn "\nParse              Failed...\n"
        putStrLn "Tokens:"
        mapM_ (putStrLn . showPosToken . mkPosToken) ts
        putStrLn err
        exitFailure
    Right tree -> do
        putStrLn "\nParsed tree\n"
        print tree
        ast <- typeCheckStage tree
        putStrLn "\nTypechecked ast\n"
        print ast
        executeStage ast
        return ()
    where
    ts = myLexer s
    showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

type PreprocessorOutput = (Code, Int, VariableId)

typeCheckStage :: A.Code -> IO PreprocessorOutput
typeCheckStage code =
    let err = runExcept $ runStateT (typeCheck code) makePreprocessorState in
    handleTypeCheckError err

handleTypeCheckError :: Either PreprocessorError (Code, PreprocessorState) -> IO PreprocessorOutput
handleTypeCheckError (Left err) = do
    putStrLn "Error in type checker"
    print err
    exitFailure
handleTypeCheckError (Right (ast, state)) = do
    putStrLn "\n"
    print state
    let PreprocessorState _ (Allocator _ _ stackSize) _ mainId = state
    return (ast, stackSize, mainId)


executeStage :: PreprocessorOutput -> IO ()
executeStage (code, stackSize, mainId) = do
    out <- runExceptT $ runStateT (execute code) (makeExecutionState stackSize mainId)
    handleExecutionError out


handleExecutionError :: Either ExecutionError ((), ExecutionState) -> IO ()
handleExecutionError (Left err) = do
    putStrLn "Error during execution"
    print err
    exitFailure
handleExecutionError (Right _) = do
    return ()

runFile :: ParseFun A.Code -> FilePath -> IO ()
runFile p f = readFile f >>= parse p
