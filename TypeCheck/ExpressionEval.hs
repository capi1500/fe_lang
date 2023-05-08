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
internalCreatePlaceExpression variableId (PlaceContext Mutable) = do
    p <- gets position
    variable <- getVariableById variableId
    when (variableMutability variable == Const) $ throw (CannotTakeMutableReferenceToConstant p variableId)
    unless (variableState variable == Free || variableState variable == Uninitialized) $ throw (AlreadyBorrowed p variableId)
    return $ PlaceType Mutable variableId
internalCreatePlaceExpression variableId (PlaceContext Const) = do
    return $ PlaceType Const variableId
internalCreatePlaceExpression variableId ValueContext = do
    variable <- getVariableById variableId
    -- if isReference (variableType variable) then do
    --     value <- makeImplicitBorrowValue variableId Const
    --     return $ ValueType value
    -- else do
    let value = setValueOwned False (variableValue variable)
    unless (isCopy (variableType variable)) $ mutateVariableById variableId (mutateVariableValue (setValueOwned False))
    moveOutOrCopyById variableId
    return $ ValueType value

createValueExpression :: Value -> PreprocessorMonad ExpressionType
createValueExpression value = gets context >>= internalCreateValueExpression value

internalCreateValueExpression :: Value -> ExpressionContext -> PreprocessorMonad ExpressionType
internalCreateValueExpression value (PlaceContext mutability) = do
    tempId <- addTemporaryVariable mutability value
    markVariableAsToDrop tempId
    return $ PlaceType mutability tempId
internalCreateValueExpression value ValueContext = do
    return $ ValueType value
