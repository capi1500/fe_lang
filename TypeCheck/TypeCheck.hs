{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module TypeCheck.TypeCheck where

import Data.Bits (xor)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Map (empty)
import Data.Foldable (traverse_)
import Control.Monad.Except
import Control.Monad.State

import qualified Fe.Abs as A
import Fe.Abs (HasPosition(hasPosition))

import Common.Ast hiding (Variable, Value)
import Common.Utils
import Common.Types
import Common.Scope

import TypeCheck.Utils
import TypeCheck.State
import TypeCheck.Error
import TypeCheck.Variable
import Data.List (intercalate)
import Common.Printer
import TypeCheck.ExpressionEval
import TypeCheck.VariablesUtils
import TypeCheck.LifetimeUtils
import TypeCheck.TypesUtils
import TypeCheck.StateUtils
import TypeCheck.BorrowCheckerUtils
import TypeCheck.Printer

class TypeCheck a b where
    typeCheck :: a -> PreprocessorMonad b

instance TypeCheck A.Type Type where
    typeCheck :: A.Type -> PreprocessorMonad Type
    typeCheck (A.TypeSimple p (A.Ident ident)) = do
        getType ident
    typeCheck (A.TypeArray _ (A.ArrayType p t)) = do
        t' <- typeCheck t
        return $ arrayType t'
    typeCheck (A.TypeModified _ modifier t) = do
        t' <- typeCheck t
        modifyType t' modifier
    typeCheck (A.TypeFunction p functionKind lifetime params returnType) = do
        throw $ Other "Type functions not yet implemented" p

instance TypeCheck A.Code Code where
    typeCheck :: A.Code -> PreprocessorMonad Code
    typeCheck (A.Code p statements) = do
        traverse_ initializeGlobalScope statements
        maybeMain <- getVariable mainFunction
        statements' <- traverse (\s -> do
            putPosition (hasPosition s)
            updateLifetime
            typeCheck s) statements
        return $ Code statements'

initializeGlobalScope :: A.Statement -> PreprocessorMonad ()
initializeGlobalScope (A.ItemStatement _ item) = addToScope item
initializeGlobalScope _ = do return ()

addToScope :: A.Item -> PreprocessorMonad ()
addToScope (A.ItemFunction p (A.Ident ident) lifetimes params returnType _) = do
    putPosition p
    params' <- traverse typeCheck params
    declaredReturnType <- typeCheck returnType
    -- lifetimes' <- typeCheck lifetimes -- TODO: check explicit lifetimes
    let t = TFunction
            (NamedFunction ident)
            Fn
            params'
            declaredReturnType
    addVariable ident Const (makeValue p t True)
    return ()
addToScope (A.ItemStruct p ident lifetimes fields) = do
    putPosition p
    throw $ Other "Structs not yet implemented" p
addToScope (A.ItemVariant p ident lifetimes types) = do
    putPosition p
    throw $ Other "Variants not yet implemented" p
addToScope (A.ItemVariable p cv (A.Ident ident) typeDeclaration initialization) = do
    putPosition p
    scope <- gets typeDefinitions
    when (isGlobal scope && isVariableCV cv) (do throw $ VariableAtGlobalScope p ident)

instance TypeCheck A.Statement Statement where
    typeCheck :: A.Statement -> PreprocessorMonad Statement
    typeCheck (A.SemicolonStatement p) = do
        return EmptyStatement
    typeCheck (A.ItemStatement p item) = do
        putPosition p
        scope <- gets typeDefinitions
        unless (isGlobal scope) $ do addToScope item
        statement <- typeCheck item
        printVariables
        endStatement
        return statement
    typeCheck (A.ExpressionStatement p expression) = do
        putPosition p
        (expression', value) <- typeCheckInValueContext expression
        
        printVariables

        dropValue value
        endStatement
        return (ExpressionStatement expression')

instance TypeCheck A.Item Statement where
    typeCheck :: A.Item -> PreprocessorMonad Statement
    -- assumes function name is added to scope
    typeCheck (A.ItemFunction p (A.Ident name) lifetimes params _ expression) = do
        putPosition p
        Variable _ _ functionType id _ _ value _ <- getVariable name
        
        let TFunction _ _ declaredParams declaredType = functionType

        let addFunctionParam = \(FunctionParam i t, p) -> do
                putPosition p
                addVariable i Const (makeValue p t True) -- function parameters are not mutable (but if they are mutable references, they still can be written to)
                return i

        putPosition p
        (expression', actualType, paramIds) <- inNewFrame $ do
                paramIds <- traverse addFunctionParam (zip declaredParams (fmap hasPosition params))
                (expression', value) <- typeCheckInValueContext expression
                -- TODO: check if any dangling reference is returned
                unless (null (borrows value) && null (borrowsMut value)) $ throw (Other "Checking for dangling references not yet implemented" p)
                dropValue value
                return (expression', valueType value, paramIds)

        assertType p declaredType actualType
        return $ NewFunctionStatement name expression' paramIds

    typeCheck (A.ItemStruct p ident lifetimes fields) = do
        putPosition p
        throw $ Other "Structs not yet implemented" p
    typeCheck (A.ItemVariant p ident lifetimes subtypes) = do
        putPosition p
        throw $ Other "Variants not yet implemented" p
    typeCheck (A.ItemVariable p cv (A.Ident name) typeDeclaration initialization) = do
        putPosition p
        when (isConstCV cv && isUnInitialized initialization) $ throw $ ConstantNotInitialized p name

        let mutability = if isConstCV cv then Const else Mutable
        declaredType <- typeCheck typeDeclaration
        initialization' <- if isInitialized initialization then do
            let A.Initialized _ expression = initialization
            (expression', value) <- typeCheckInValueContext expression

            let initializedType = valueType value

            when (declaredType /= TUntyped) $ assertType p declaredType initializedType

            -- lifetime <- getLifetime
            -- unless (isSubLifetime initializationLifetime lifetime) $ throw (LifetimesMismatch p (hasPosition expression) lifetime initializationLifetime)

            addVariable name mutability (setValueOwned True value)
            -- let expression'' = if isNothing (variableName variable) then
            --         expression'
            --     else
            --         DereferenceExpression expression'
            return $ VarInitialized expression'
        else do
            addUninitializedVariable name mutability declaredType
            return VarUninitialized
        
        return $ NewVariableStatement name initialization'

instance TypeCheck A.FunctionParam FunctionParam where
    typeCheck :: A.FunctionParam -> PreprocessorMonad FunctionParam
    typeCheck (A.Parameter p (A.Ident ident) t) = do
        t' <- typeCheck t
        return $ FunctionParam ident t'

instance TypeCheck A.FunctionReturnType Type where
    typeCheck :: A.FunctionReturnType -> PreprocessorMonad Type
    typeCheck (A.ReturnValue p t) = do
        typeCheck t
    typeCheck (A.ReturnUnit p) = do
        return unitType

instance TypeCheck A.TypeDeclaration Type where
    typeCheck :: A.TypeDeclaration -> PreprocessorMonad Type
    typeCheck (A.Untyped _) = do
        return TUntyped
    typeCheck (A.Typed _ t) = do
        typeCheck t

instance TypeCheck A.Expression TypedExpression where
    typeCheck :: A.Expression -> PreprocessorMonad TypedExpression
    typeCheck (A.BlockExpression p statements) = do
        putPosition p
        (statements', blockValue) <- inNewScope $ do
                if null statements then do
                    return ([], makeValue p unitType False)
                else do
                    let last:prefix = reverse statements
                    prefix' <- traverse (\s -> do
                            putPosition (hasPosition s)
                            updateLifetime
                            typeCheck s)
                            (reverse prefix)

                    let handler = \case
                            A.ExpressionStatement p expression -> do
                                putPosition p
                                (expression', blockValue) <- typeCheckInValueContext expression

                                printVariables

                                endStatement
                                return (ExpressionStatement expression', blockValue)
                            s -> do
                                s' <- typeCheck s
                                return (s', makeValue p unitType False)
                    
                    putPosition (hasPosition last)
                    updateLifetime
                    (last', blockValue) <- handler last

                    return (listPushBack last' prefix', blockValue)

        -- don't care about lifetimes, dropping values at the end of the block should catch dangling references        
        et <- createValueExpression blockValue
        return $ TypedExpression (BlockExpression statements') et
    typeCheck (A.GroupedExpression _ expression) = do
        typeCheck expression
    typeCheck (A.VariableExpression p (A.Ident name)) = do
        putPosition p
        context <- gets context
        variable <- getVariable name
        let id = variableId variable
        when (variableState variable == Moved) $ throw (UseAfterMoved p id)
        
        let expression = case context of
              PlaceContext _ -> ReferenceExpression
              ValueContext -> VariableExpression
        et <- createPlaceExpression id
        return $ TypedExpression (expression name) et
    typeCheck (A.IfExpression p ifExpression) = do
        typeCheck ifExpression
    typeCheck (A.CallExpression p function params) = do
        (functionId, function') <- do
            (function', id) <- typeCheckInPlaceContext Const function
            return (id, function')
        
        borrowedValue <- makeImplicitBorrowValue functionId Const
        let TReference Const functionType = valueType borrowedValue
        unless (isFunction functionType) $ do throw (ExpressionNotCallable p functionType)

        let TFunction _ kind declaredParams returnType = functionType
        when (length params /= length declaredParams) $ throw (WrongNumberOfParams p functionType)
        let paramCheck = \(FunctionParam paramIdent paramDeclaredType, A.CallParam _ param) -> do
            (paramExpression, paramValue) <- typeCheckInValueContext param
            assertType (hasPosition param) (valueType paramValue) paramDeclaredType 
            dropValue paramValue -- TODO: for now, drop, transfer to return value when allowing for explicit lifetimes
            return (paramIdent, paramExpression)

        params' <- traverse paramCheck (zip declaredParams params)
        dropValue borrowedValue
        -- TODO: for now, only static expressions (temporary and moved values) are returned
        et <- createValueExpression (makeValue p returnType False)
        return $ TypedExpression (CallExpression function' params') et 
    typeCheck (A.UnaryExpression _ (A.UnaryMinus p) e) = do
        e' <- do
            (e', v) <- typeCheckInValueContext e
            assertType (hasPosition e) (valueType v) i32Type
            return e'
        expressionType <- createValueExpression (makeValue p i32Type False)
        return $ TypedExpression (UnaryMinusExpression e') expressionType
    typeCheck (A.UnaryExpression _ (A.UnaryNegation p) e) = do
        e' <- do
            (e', v) <- typeCheckInValueContext e
            assertType (hasPosition e) (valueType v) i32Type
            return e'
        expressionType <- createValueExpression (makeValue p boolType False)
        return $ TypedExpression (UnaryNegationExpression e') expressionType
    typeCheck (A.UnaryExpression _ (A.Dereference p) e) = do
        (e', placeId) <- withinContext $ do
            context <- gets context
            putContext (case context of
                PlaceContext mutability -> PlaceContext mutability
                ValueContext -> PlaceContext Const)
            putPosition (hasPosition e)

            TypedExpression e' et <- typeCheck e
            let PlaceType mutability placeId = et
            return (e', placeId)
        
        borrowedVariable <- getVariableById placeId
        addWarning $ Debug ("Attempting to deref " ++ codePrint 1 borrowedVariable)
        
        let t = variableType borrowedVariable
        unless (isReference t) $ throw (CannotDerefNotReference p t)

        let borrowedValue = variableValue borrowedVariable
        let borrows' = borrows borrowedValue
        let borrowsMut' = borrowsMut borrowedValue
        -- unless (length borrows' + length borrowsMut' == 1) $ throw (CannotDerefReferenceToMultipleVariables p)

        let derefedPlaceId = if null borrows' then
                head borrowsMut'
            else
                head borrows'
        moveOut borrowedVariable
        context <- gets context
        et <- if isValueContext context then do
            id <- makeImplicitBorrowValue derefedPlaceId Const
            et <- createPlaceExpression derefedPlaceId
            dropValue id
            return et
        else do
            createPlaceExpression derefedPlaceId

        return $ TypedExpression (DereferenceExpression e') et
    typeCheck (A.UnaryExpression _ (A.Reference p) e) = do
        (e', borrowedId) <- typeCheckInPlaceContext Const e
        v <- makeBorrow borrowedId Const
        et <- createValueExpression v
        return $ TypedExpression e' et
    typeCheck (A.UnaryExpression _ (A.ReferenceMut p) e) = do
        (e', borrowedId) <- typeCheckInPlaceContext Mutable e
        v <- makeBorrow borrowedId Mutable
        et <- createValueExpression v
        return $ TypedExpression e' et
    typeCheck (A.LiteralExpression p literal) = do
        putPosition p
        typeCheck literal
    typeCheck (A.PlusExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 Plus
    typeCheck (A.MinusExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 Minus
    typeCheck (A.MultiplyExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 Multiply
    typeCheck (A.DivideExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 Divide
    typeCheck (A.ModuloExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 Modulo
    typeCheck (A.LShiftExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 LShift
    typeCheck (A.RShiftExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 RShift
    typeCheck (A.BitOrExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 BitOr
    typeCheck (A.BitXOrExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 BitXor
    typeCheck (A.BitAndExpression p e1 e2) = do
        makeI32DoubleOperatorExpression p e1 e2 BitAnd
    typeCheck (A.ComparisonExpression p e1 operator e2) = do
        putPosition p
        makeComparisonOperatorExpression operator e1 e2
    typeCheck (A.AssignmentExpression p e1 operator e2) = do
        putPosition p
        makeAssignmentExpression operator e1 e2
    typeCheck x = do
        throw $ Other "Expression not yet implemented" (hasPosition x)

makeI32DoubleOperatorExpression :: A.BNFC'Position -> A.Expression -> A.Expression -> NumericDoubleOperator  -> PreprocessorMonad TypedExpression
makeI32DoubleOperatorExpression p e1 e2 operator = do
    (e1', v1) <- typeCheckInValueContext e1
    (e2', v2) <- typeCheckInValueContext e2
    assertType (hasPosition e1) (valueType v1) i32Type
    assertType (hasPosition e2) (valueType v1) i32Type
    expressionType <- createValueExpression (makeValue p i32Type False)
    return $ TypedExpression (I32DoubleOperatorExpression operator e1' e2') expressionType

makeComparisonOperatorExpression :: A.ComparisonOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeComparisonOperatorExpression (A.Equals p) e1 e2 = do
    (e1', id1) <- typeCheckInPlaceContext Const e1
    v1 <- makeImplicitBorrowValue id1 Const
    (e2', id2) <- typeCheckInPlaceContext Const e2
    v2 <- makeImplicitBorrowValue id2 Const

    printVariables
    assertType p (valueType v1) (valueType v2)

    let TReference _ innerT = valueType v1
    when (isFunction innerT) $ throw (CannotComapreFunctions innerT)

    expressionType <- createValueExpression (makeValue p boolType False)
    dropValue v1
    dropValue v2
    return $ TypedExpression (BoolDoubleOperatorExpression Equals e1' e2') expressionType
makeComparisonOperatorExpression (A.Greater p) e1 e2 = do
    makeI32ComparisonExpression e1 e2 Greater
makeComparisonOperatorExpression (A.Smaller p) e1 e2 = do
    makeI32ComparisonExpression e1 e2 Smaller
makeComparisonOperatorExpression (A.NotEquals p) e1 e2 = do
    TypedExpression e' et <- makeComparisonOperatorExpression (A.Equals p) e1 e2
    return $ TypedExpression (UnaryNegationExpression e') et
makeComparisonOperatorExpression (A.GreaterEquals p) e1 e2 = do
    TypedExpression e' et <- makeI32ComparisonExpression e1 e2 Smaller
    return $ TypedExpression (UnaryNegationExpression e') et
makeComparisonOperatorExpression (A.SmallerEquals p) e1 e2 = do
    TypedExpression e' et <- makeI32ComparisonExpression e1 e2 Greater
    return $ TypedExpression (UnaryNegationExpression e') et

makeI32ComparisonExpression :: A.Expression -> A.Expression -> BooleanDoubleOperator -> PreprocessorMonad TypedExpression
makeI32ComparisonExpression e1 e2 operator = do
    (e1', id1) <- typeCheckInPlaceContext Const e1
    v1 <- makeImplicitBorrowValue id1 Const
    (e2', id2) <- typeCheckInPlaceContext Const e2
    v2 <- makeImplicitBorrowValue id2 Const
    printVariables

    assertType (hasPosition e1) (valueType v1) (TReference Const i32Type) -- here its a reference to int vs int
    assertType (hasPosition e2) (valueType v2) (TReference Const i32Type)
    expressionType <- createValueExpression (makeValue (hasPosition e1) boolType False)
    dropValue v1
    dropValue v2
    printVariables
    return $ TypedExpression (BoolDoubleOperatorExpression operator e1' e2') expressionType

makeAssignmentExpression :: A.AssignmentOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeAssignmentExpression (A.PlusEqual p) e1 e2 = do makeCompoundAssignmentExpression Plus e1 e2
makeAssignmentExpression (A.MinusEqual p) e1 e2 = do makeCompoundAssignmentExpression Minus e1 e2
makeAssignmentExpression (A.MultiplyEqual p) e1 e2 = do makeCompoundAssignmentExpression Multiply e1 e2
makeAssignmentExpression (A.DivideEqual p) e1 e2 = do makeCompoundAssignmentExpression Divide e1 e2
makeAssignmentExpression (A.ModuloEqual p) e1 e2 = do makeCompoundAssignmentExpression Modulo e1 e2
makeAssignmentExpression (A.AndEqual p) e1 e2 = do makeCompoundAssignmentExpression BitAnd e1 e2
makeAssignmentExpression (A.OrEqual p) e1 e2 = do makeCompoundAssignmentExpression BitOr e1 e2
makeAssignmentExpression (A.XorEqual p) e1 e2 = do makeCompoundAssignmentExpression BitXor e1 e2
makeAssignmentExpression (A.LShiftEqual p) e1 e2 = do makeCompoundAssignmentExpression LShift e1 e2
makeAssignmentExpression (A.RShiftEqual p) e1 e2 = do makeCompoundAssignmentExpression RShift e1 e2
makeAssignmentExpression (A.Assign p) e1 e2 = do
    (e1', placeId) <- typeCheckInPlaceContext Mutable e1
    place <- getVariableById placeId
    let t1 = variableType place
    when (variableState place == Free) $ dropValue (variableValue place)
    
    (e2', newValue) <- typeCheckInValueContext e2    
    let t2 = valueType newValue

    assertType p t1 t2

    mutateVariableById placeId (setVariableState Free)
    mutateVariableById placeId (mutateVariableValue (const newValue))
    printVariables

    et <- createValueExpression (makeValue (hasPosition e1) unitType False)
    return $ TypedExpression (AssignmentExpression e1' e2') et

makeCompoundAssignmentExpression :: NumericDoubleOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeCompoundAssignmentExpression op e1 e2 = do
    throw $ Other "Compound assignment not yet implemented" (hasPosition e1)
    -- TypedExpression e1' t _ <- typeCheck e1
    -- TypedExpression e2' t _ <- typeCheck e2
    -- return $ TypedExpression (AssignmentExpression False e1' (I32DoubleOperatorExpression op e1' e2')) unitType staticLifetime

instance TypeCheck A.IfExpression TypedExpression where
    typeCheck :: A.IfExpression -> PreprocessorMonad TypedExpression
    typeCheck (A.If p condition onTrue) =
        ifExpression p condition onTrue (Nothing :: Maybe A.Expression)
    typeCheck (A.IfElse p condition onTrue onFalse) =
        ifExpression p condition onTrue (Just onFalse)
    typeCheck (A.IfElseIf p condition onTrue onFalse) =
        ifExpression p condition onTrue (Just onFalse)

ifExpression :: (TypeCheck a TypedExpression, HasPosition a) => A.BNFC'Position -> A.Expression -> A.Expression -> Maybe a -> PreprocessorMonad TypedExpression
ifExpression p condition onTrue onFalse = do
    condition' <- do
        (condition', conditionValue) <- typeCheckInValueContext condition
        assertType (hasPosition condition) (valueType conditionValue) boolType
        return condition'

    (onTrue', onTrueValue) <- do
        (onTrue', onTrueValue) <- typeCheckInValueContext onTrue
        addWarning $ Debug "onTrue"
        printVariables
        return (onTrue', onTrueValue)
    let onTrueType = valueType onTrueValue

    (onFalse', onFalseValue) <- do
        if isNothing onFalse then do
            return (Nothing, makeValue p unitType False)
        else do
            (onFalse', onFalseValue) <- typeCheckInValueContext (fromJust onFalse)
            addWarning $ Debug "onFalse"
            printVariables
            return (Just onFalse', onFalseValue)
    let onFalseType = valueType onFalseValue

    t <- if isFunction onTrueType && isFunction onFalseType then do
        mergeFunctionTypesOrThrow (hasPosition onTrue) onTrueType onFalseType
    else do
        assertType (hasPosition onTrue) onTrueType onFalseType
        return onTrueType

    let value = Value {
        valueCreatedAt = p,
        valueType = t,
        borrows = borrows onTrueValue ++ borrows onFalseValue,
        borrowsMut = borrowsMut onTrueValue ++ borrowsMut onFalseValue,
        owned = False
    }
    putPosition p
    traverse_ borrow (borrows value)
    traverse_ borrowMut (borrowsMut value)
    et <- createValueExpression value
    return $ TypedExpression (IfExpression condition' onTrue' onFalse') et

instance TypeCheck A.Literal TypedExpression where
    typeCheck :: A.Literal -> PreprocessorMonad TypedExpression
    typeCheck (A.LiteralChar p char) = do
        valueExpression <- createValueExpression (makeValue p charType False)
        return $ TypedExpression (LiteralExpression $ VChar char) valueExpression
    typeCheck (A.LiteralString p string) = do
        valueExpression <- createValueExpression (makeValue p stringType False)
        return $ TypedExpression (MakeArrayExpression (fmap VChar string)) valueExpression
    typeCheck (A.LiteralInteger p integer) = do
        valueExpression <- createValueExpression (makeValue p i32Type False)
        return $ TypedExpression (LiteralExpression $ VI32 (fromIntegral integer)) valueExpression
    typeCheck (A.LiteralBoolean p (A.BoolTrue _)) = do
        valueExpression <- createValueExpression (makeValue p boolType False)
        return $ TypedExpression (LiteralExpression $ VBool True) valueExpression
    typeCheck (A.LiteralBoolean p (A.BoolFalse _)) = do
        valueExpression <- createValueExpression (makeValue p boolType False)
        return $ TypedExpression (LiteralExpression $ VBool False) valueExpression

instance TypeCheck A.CallParam TypedExpression where
    typeCheck :: A.CallParam -> PreprocessorMonad TypedExpression
    typeCheck (A.CallParam _ expression) = do
        typeCheck expression

typeCheckInValueContext :: (TypeCheck a TypedExpression, HasPosition a) => a -> PreprocessorMonad (Expression, Value)
typeCheckInValueContext e = withinContext $ do
    putContext ValueContext
    putPosition (hasPosition e)
    TypedExpression e' et <- typeCheck e
    let ValueType v = et
    return (e', v)

typeCheckInPlaceContext :: (TypeCheck a TypedExpression, HasPosition a) => Mutable -> a -> PreprocessorMonad (Expression, VariableId)
typeCheckInPlaceContext mutable e = withinContext $ do
    putContext $ PlaceContext mutable
    putPosition (hasPosition e)
    TypedExpression e' et <- typeCheck e
    let PlaceType _ v = et
    return (e', v)
