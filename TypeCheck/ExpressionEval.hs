module TypeCheck.ExpressionEval where

import Prelude hiding (null)
import Data.Foldable hiding (null)
import Data.Set
import Control.Monad.State

import Fe.Abs (BNFC'Position)

import Common.Types

import TypeCheck.State
import TypeCheck.Variable
import TypeCheck.Error
import TypeCheck.VariablesUtils
import TypeCheck.BorrowCheckerUtils    

-- strong guarantee - value got inside a context (ie. place context) is of a coresponding type (ie. place type)
    -- value type holds unowned value, if it is not a primitive value, it needs to be droped/transfered on use

createPlaceExpression :: VariableId -> PreprocessorMonad ExpressionType
createPlaceExpression variableId = gets context >>= internalCreatePlaceExpression variableId

internalCreatePlaceExpression :: VariableId -> ExpressionContext -> PreprocessorMonad ExpressionType
internalCreatePlaceExpression variableId (PlaceContext _) = do
    return $ PlaceType variableId
internalCreatePlaceExpression variableId ValueContext = do
    variable <- getVariableById variableId
    let value = setValueOwned False (variableValue variable)
    
    shouldMove <- canMoveById variableId
    when shouldMove $ mutateVariableById variableId (mutateVariableValue (const value))
    moveOutOrCopyById variableId
    return $ ValueType value

createValueExpression :: Value -> PreprocessorMonad ExpressionType
createValueExpression value = gets context >>= internalCreateValueExpression value

internalCreateValueExpression :: Value -> ExpressionContext -> PreprocessorMonad ExpressionType
internalCreateValueExpression value (PlaceContext _) = do
    tempId <- addTemporaryVariable Mutable value
    markVariableAsToDrop tempId
    return $ PlaceType tempId
internalCreateValueExpression value ValueContext = do
    return $ ValueType value
