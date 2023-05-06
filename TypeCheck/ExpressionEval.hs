module TypeCheck.ExpressionEval where

import Prelude hiding (null)
import Data.Foldable hiding (null)
import Data.Set
import Control.Monad.State

import Common.Types

import TypeCheck.State
import TypeCheck.Variable
import TypeCheck.Error
import TypeCheck.VariablesUtils
import Fe.Abs (BNFC'Position)
import TypeCheck.BorrowCheckerUtils

evalExpressionInValueContext :: ExpressionType -> PreprocessorMonad Value
evalExpressionInValueContext (PlaceType variableId) = do
    putContext ValueContext
    variable <- getVariableById variableId
    handlePlaceExpression variableId
    return (variableValue variable)
evalExpressionInValueContext (ValueType v) = return v
evalExpressionInValueContext AssigneeType = 
    throw $ Other "Not yet implented" Nothing

handlePlaceExpression :: VariableId -> PreprocessorMonad ()
handlePlaceExpression variableId = do
    variable <- getVariableById variableId
    gets context >>= internalHandlePlaceExpression variableId variable


internalHandlePlaceExpression :: VariableId -> Variable -> ExpressionContext -> PreprocessorMonad ()
internalHandlePlaceExpression id variable (PlaceContext _) = do
    throw $ Other "Not yet implented" Nothing
internalHandlePlaceExpression id variable ValueContext = do
    tryMoveOut id variable

createValueExpression :: Value -> PreprocessorMonad ExpressionType
createValueExpression value = gets context >>= internalCreateValueExpression value

internalCreateValueExpression :: Value -> ExpressionContext -> PreprocessorMonad ExpressionType
internalCreateValueExpression value (PlaceContext _) = do
    tempId <- addTemporaryVariable Mutable value
    markVariableAsToDrop tempId
    return $ PlaceType tempId
internalCreateValueExpression value ValueContext = do
    return $ ValueType value
