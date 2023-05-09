module TypeCheck.ExpressionEval where

import Prelude hiding (null)
import Data.Foldable hiding (null)
import Data.Set
import Data.Maybe
import Control.Monad.State

import Fe.Abs (BNFC'Position)

import Common.Types

import TypeCheck.State
import TypeCheck.Variable
import TypeCheck.Error
import TypeCheck.VariablesUtils
import TypeCheck.BorrowCheckerUtils
import TypeCheck.Utils
import Control.Monad.Except
import TypeCheck.Printer (printDebug)
import Common.Ast (Expression (DereferenceExpression))
import Common.Printer

-- strong guarantee - value got inside a context (ie. place context) is of a corresponding type (ie. place type)
    -- value type holds unowned value, if it is not a primitive value, it needs to be dropped/transferred on use

-- createValueExpression can auto-dereference references, if it expects a type
-- createPlaceExpression in ValueContext, if it expects a type, attempts to dereference the variable exactly once

createPlaceExpression :: VariableId -> PreprocessorMonad (Expression -> Expression, ExpressionType)
createPlaceExpression variableId = gets expressionContext >>= internalCreatePlaceExpression variableId

internalCreatePlaceExpression :: VariableId -> ExpressionContext -> PreprocessorMonad (Expression -> Expression, ExpressionType)
internalCreatePlaceExpression variableId (PlaceContext Mutable) = do
    p <- gets position
    variable <- getVariableById variableId
    when (variableMutability variable == Const) $ throw (CannotTakeMutableReferenceToConstant p variableId)
    unless (variableState variable == Free || variableState variable == Uninitialized) $ throw (AlreadyBorrowed p variableId)
    return (id, PlaceType Mutable variableId)
internalCreatePlaceExpression variableId (PlaceContext Const) = do
    return (id, PlaceType Const variableId)
internalCreatePlaceExpression variableId (ValueContext expectedType) = do
    variable <- getVariableById variableId
    -- if I expect a &[i32] but have &mut [i32]
        -- I should be able to use it, no move required
    -- if I expect a i32 but have &i32
        -- I should be able to use it, copy
    -- if I expect a [i32] but have &[i32]
        -- I shouldn't be able to use it

    -- if expected type T is copy, but &T or &mut T given
        -- deref and pass value inside. its a copy, so no borrows/owned values attached
    -- if expected type is &T, but &T or &mut T given
        -- do nothing, just return the value like a copy. But it's a value expression, so returned value will be dropped.
    if isJust expectedType then do
        let Just t = expectedType
        if isCopy t && canSubstitute (variableType variable) (TReference Const t) then do
            do {
                derefedId <- deref (variableValue variable);
                derefedVariable <- getVariableById derefedId;
                printDebug $ "implicit deref " ++ codePrint 1 derefedVariable;
                return (DereferenceExpression, ValueType (variableValue derefedVariable))
            } `catchError` handler variable
        else do
            def variable
    else def variable
  where
    def variable = do
        p <- gets position
        forM_ expectedType (assertType p (variableType variable))
        let value = setValueOwned False (variableValue variable)
        unless (isCopy (variableType variable)) $ mutateVariableById variableId (mutateVariableValue (setValueOwned False))
        moveOutOrCopyById variableId
        return (id, ValueType value)
    handler variable (CannotDerefReferenceToMultipleVariables _) = def variable
    handler variable e = throwError e

createValueExpression :: Value -> PreprocessorMonad ExpressionType
createValueExpression value = gets expressionContext >>= internalCreateValueExpression value

internalCreateValueExpression :: Value -> ExpressionContext -> PreprocessorMonad ExpressionType
internalCreateValueExpression value (PlaceContext mutability) = do
    tempId <- addTemporaryVariable mutability value
    markVariableAsToDrop tempId
    return $ PlaceType mutability tempId
internalCreateValueExpression value (ValueContext expectedType) = do
    p <- gets position
    forM_ expectedType (assertType p (valueType value))
    return $ ValueType value
