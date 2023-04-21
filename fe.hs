import Prelude  (($), (.), Either(..), Int, (>), String, (++), concat, unlines, Show, show, IO, (>>), (>>=), mapM_, putStrLn, FilePath, getContents, readFile, print, Num ((+)), Integer)

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when, return )

import Fe.Abs
import Fe.Lex   ( Token, mkPosToken )
import Fe.Par   ( pCode, myLexer )
import Fe.Print ( Print, printTree )
import Fe.Skel  ()
import Control.Applicative (Alternative(empty))
import TypeCheck.TypeCheck (typeCheck)
import Control.Monad.State (runState, StateT (runStateT), MonadIO (liftIO))
import Control.Monad.Except (runExceptT, runExcept)
import Data.Either
import Exec.Exec
import Common.Ast (Annotation)
import TypeCheck.Utils (TypeCheckError, TypeCheckState, makeEmptyTypeCheckState)

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

parse :: ParseFun Code -> String -> IO ()
parse p s =
    case p ts of
    Left err -> do
        putStrLn "\nParse              Failed...\n"
        putStrLn "Tokens:"
        mapM_ (putStrLn . showPosToken . mkPosToken) ts
        putStrLn err
        exitFailure
    Right tree -> do
        ast <- typeCheckStage tree
        putStrLn "\nParsed tree\n"
        print tree
        putStrLn "\nTypechecked ast\n"
        print ast
        x <- runExceptT $ runStateT (run ast) 0
        return ()
    where
    ts = myLexer s
    showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]


typeCheckStage :: Code -> IO (Code' Annotation)
typeCheckStage code =
    let err = runExcept $ runStateT (typeCheck code) makeEmptyTypeCheckState in
    handleTypeCheckError err

handleTypeCheckError :: Either TypeCheckError (Code' Annotation, TypeCheckState) -> IO (Code' Annotation)
handleTypeCheckError (Left err) = do
    putStrLn "Error in type checker"
    print err
    exitFailure
handleTypeCheckError (Right (ast, state)) = do
    putStrLn "\n"
    print state
    return ast


executeStage :: Code' Annotation -> IO ()
executeStage code = do
    out <- runExceptT $ runStateT (run code) 0
    handleExecutionError out


handleExecutionError :: Either ExecutionError ((), ExecutionState) -> IO ()
handleExecutionError (Left err) = do
    putStrLn "Error during execution"
    print err
    exitFailure
handleExecutionError (Right _) = do
    return ()

runFile :: ParseFun Code -> FilePath -> IO ()
runFile p f = readFile f >>= parse p
