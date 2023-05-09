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
import Data.List (intercalate)
import Control.Monad.Except
import Control.Monad.State

import qualified Fe.Abs as A
import Fe.Abs (HasPosition(hasPosition))

import Common.Ast hiding (variables, position, Other, Uninitialized, Variable, Value)
import Common.Utils
import Common.Types
import Common.Scope
import Common.Printer

import TypeCheck.Utils
import TypeCheck.State
import TypeCheck.Error
import TypeCheck.Variable

import TypeCheck.ExpressionEval
import TypeCheck.VariablesUtils
import TypeCheck.LifetimeUtils
import TypeCheck.TypesUtils
import TypeCheck.StateUtils
import TypeCheck.BorrowCheckerUtils
import TypeCheck.Printer
import TypeCheck.ValueUtils
import TypeCheck.ContextUtils

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
        functionKind' <- typeCheck functionKind
        typeCheck lifetime :: PreprocessorMonad ()
        params' <- traverse typeCheck params
        returnType' <- typeCheck returnType
        return $ TFunction functionKind' params' returnType'

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
    let t = TFunction Fn params' declaredReturnType
    makeValue p t True >>= addVariable ident Const
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
        -- printVariables
        endStatement
        return statement
    typeCheck (A.ExpressionStatement p expression) = do
        putPosition p
        (expression', value) <- typeCheckInValueContext Nothing expression
        -- printVariables
        dropValue value
        endStatement
        return (ExpressionStatement expression')

instance TypeCheck A.Item Statement where
    typeCheck :: A.Item -> PreprocessorMonad Statement
    -- assumes function name is added to scope
    typeCheck (A.ItemFunction p (A.Ident name) lifetimes params _ expression) = do
        putPosition p
        Variable _ _ functionType id _ _ value _ <- getVariable name

        let TFunction _ declaredParams declaredType = functionType

        let addFunctionParam = \(t, A.Parameter p (A.Ident name) _) -> do
                putPosition p
                makeValue p t True >>= addVariable name Mutable
                return name

        putPosition p
        (expression', actualType, paramIds) <- inNewFrame $ do
                setCurrentFunctionReturnType declaredType
                paramIds <- traverse addFunctionParam (zip declaredParams params)
                (expression', value) <- typeCheckInValueContext Nothing expression
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
            (expression', value) <- typeCheckInValueContext Nothing expression

            let initializedType = valueType value

            when (declaredType /= TUntyped) $ assertType p declaredType initializedType

            addVariable name mutability (setValueOwned True value)
            return $ VarInitialized expression'
        else do
            id <- addVariable name mutability (Value declaredType [] [] [] True)
            mutateVariableById id (setVariableState Uninitialized)
            return VarUninitialized

        return $ NewVariableStatement name initialization'

instance TypeCheck A.FunctionParam (Identifier, Type) where
    typeCheck :: A.FunctionParam -> PreprocessorMonad (Identifier, Type)
    typeCheck (A.Parameter p (A.Ident name) t) = do
        t' <- typeCheck t
        return (name, t')

instance TypeCheck A.FunctionParam Type where
    typeCheck :: A.FunctionParam -> PreprocessorMonad Type
    typeCheck (A.Parameter p _ t) = typeCheck t

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
                    value <- makeValue p unitType False
                    return ([], value)
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
                                (expression', blockValue) <- typeCheckInValueContext Nothing expression

                                -- printVariables

                                endStatement
                                return (ExpressionStatement expression', blockValue)
                            s -> do
                                s' <- typeCheck s
                                value <- makeValue p unitType False
                                return (s', value)

                    putPosition (hasPosition last)
                    updateLifetime
                    (last', blockValue) <- handler last

                    return (listPushBack last' prefix', blockValue)

        -- don't care about lifetimes, dropping values at the end of the block should catch dangling references
        et <- createValueExpression blockValue
        return $ TypedExpression (BlockExpression statements') et
    typeCheck (A.GroupedExpression _ expression) = do
        typeCheck expression
    typeCheck (A.IfExpression p ifExpression) = do
        typeCheck ifExpression
    typeCheck (A.WhileExpression p condition block) = do
        condition' <- do
            putPosition (hasPosition condition)
            (condition', conditionValue) <- typeCheckInValueContext (Just boolType) condition
            return condition'
        setInsideLoopExpression True
        (block', blockValue) <- typeCheckInValueContext Nothing block
        -- TODO: should disable warnings, but not errors
        (block', blockValue) <- typeCheckInValueContext Nothing block -- check if block can be executed multiple times (so no move happens inside etc.)
        setInsideLoopExpression False
        et <- makeValue p unitType False >>= createValueExpression
        return $ TypedExpression (WhileExpression condition' block') et
    typeCheck (A.VariableExpression p (A.Ident name)) = do
        putPosition p
        context <- gets expressionContext
        Variables mappings _ <- gets variables
        variable <- getVariable name
        let id = variableId variable
        when (isValueContext context && variableState variable == Uninitialized) $ throw (UninitializedVariableUsed p id)
        when (variableState variable == Moved) $ throw (UseAfterMoved p id)

        (mod, et) <- createPlaceExpression id
        let expression = mod . VariableExpression
        return $ TypedExpression (expression name) et
    typeCheck (A.ArrayExpressionItems p elements) = do
        when (null elements) $ throw (CannotInferType p)
        elements' <- traverse (typeCheckInValueContext Nothing) elements
        let expressions = fmap fst elements'
        let values = fmap snd elements'
        let t = valueType (head values)
        traverse_ (\t2 -> do unless (t == t2) $ throw (TypeMismatch p t t2)) (fmap valueType values)

        et <- makeValue p (TArray t) False >>= createValueExpression
        return $ TypedExpression (MakeArrayExpression expressions) et
    typeCheck (A.ArrayExpressionDefault p e1 e2) = do
        (e1', v1') <- typeCheckInValueContext (Just i32Type) e1
        (e2', v2') <- typeCheckInValueContext Nothing e2

        let innerType = valueType v2'
        et <- makeValue p (TArray innerType) False >>= createValueExpression

        return $ TypedExpression (MakeArrayDefaultsExpression e1' e2') et
    typeCheck (A.ClosureExpression p captures params returnType e) = do
        -- captures' <- traverse typeCheck captures
        params' <- traverse typeCheck params
        returnType' <- typeCheck returnType

        let addFunctionParam = \(name, t) -> do
                putPosition p
                makeValue p t True >>= addVariable name Mutable

        let paramTypes = fmap fst params'
        let paramIds = fmap snd params'
        putPosition p
        (expression', actualType) <- inNewFrame $ do
                setCurrentFunctionReturnType returnType'
                traverse_ addFunctionParam params'
                (expression', value) <- typeCheckInValueContext Nothing e
                unless (null (borrows value) && null (borrowsMut value)) $ throw (Other "Checking for dangling references not yet implemented" p)
                dropValue value
                return (expression', valueType value)

        let kind = Fn
        et <- makeValue p (TFunction kind paramIds returnType') False >>= createValueExpression
        return $ TypedExpression (MakeClosureExpression paramTypes expression') et
    typeCheck (A.CallExpression p function params) = do
        (functionId, function') <- do
            (function', id) <- typeCheckInPlaceContext Const function
            return (id, function')

        borrowedValue <- makeImplicitBorrowValue functionId Const
        let TReference Const functionType = valueType borrowedValue
        unless (isFunction functionType) $ do throw (ExpressionNotCallable p functionType)

        let TFunction kind declaredParams returnType = functionType
        when (length params /= length declaredParams) $ throw (WrongNumberOfParams p functionType)
        let paramCheck = \(paramDeclaredType, A.CallParam _ param) -> do
            -- printDebug ("Param type: " ++ codePrint 1 paramDeclaredType)
            (paramExpression, paramValue) <- typeCheckInValueContext (Just paramDeclaredType) param
            -- printDebug ("Passing value: " ++ codePrint 1 paramValue)
            dropValue paramValue -- TODO: for now, drop, transfer to return value when allowing for explicit lifetimes
            return paramExpression

        params' <- traverse paramCheck (zip declaredParams params)
        dropValue borrowedValue
        -- TODO: for now, only static expressions (temporary and moved values) are returned
        et <- makeValue p returnType False >>= createValueExpression
        return $ TypedExpression (CallExpression p function' params') et
    typeCheck (A.IndexExpression p e1 e2) = do
        (e1', placeId, mut) <- withinContext $ do
            context <- gets expressionContext
            let mut = (case context of
                    PlaceContext mutability -> mutability
                    ValueContext _ -> Const)
            putExpressionContext $ PlaceContext mut
            putPosition (hasPosition e1)
            TypedExpression e' et <- typeCheck e1
            let PlaceType _ placeId = et
            return (e', placeId, mut)

        place <- getVariableById placeId
        -- Just as with methods, Rust will also insert dereference operations on a repeatedly to find an implementation. AKA remember the dereference level, AKA don't borrow if it's a reference
        (mod1, arrayValue) <- stripReferences (variableValue place)
        let t = valueType arrayValue

        f <- if isArray (variableType place) then do
                borrow <- makeImplicitBorrowValue placeId mut
                return (\() -> dropValue borrow)
            else do
                return (\() -> return ())
        unless (isArray t) $ throw (TypeNotIndexable (hasPosition e1) t)
        let TArray innerT = t

        let indexPlaceId = head $ ownedPlaces arrayValue
        (mod, et) <- createPlaceExpression indexPlaceId

        context <- gets expressionContext
        let mod2 = mod . case context of
              PlaceContext _ -> id
              ValueContext _ -> id

        (e2', value) <- typeCheckInValueContext (Just i32Type) e2

        f ()
        let exp = mod2 (IndexExpression p (mod1 e1') e2')
        return $ TypedExpression exp et
    typeCheck (A.UnaryExpression _ (A.UnaryMinus p) e) = do
        e' <- do
            (e', v) <- typeCheckInValueContext (Just i32Type) e
            return e'
        expressionType <- makeValue p i32Type False >>= createValueExpression
        return $ TypedExpression (UnaryMinusExpression e') expressionType
    typeCheck (A.UnaryExpression _ (A.UnaryNegation p) e) = do
        e' <- do
            (e', v) <- typeCheckInValueContext (Just boolType) e
            return e'
        expressionType <- makeValue p boolType False >>= createValueExpression
        return $ TypedExpression (UnaryNegationExpression e') expressionType
    typeCheck (A.UnaryExpression _ (A.Dereference p) e) = do
        (e', placeId, mutability) <- withinContext $ do
            context <- gets expressionContext
            putExpressionContext (case context of
                PlaceContext mutability -> PlaceContext mutability
                ValueContext _ -> PlaceContext Const)
            putPosition (hasPosition e)

            TypedExpression e' et <- typeCheck e
            let PlaceType mutability placeId = et
            return (e', placeId, mutability)

        context <- gets expressionContext
        borrowedVariable <- getVariableById placeId

        let t = variableType borrowedVariable
        unless (isReference t) $ throw (CannotDerefNotReference p t)

        derefedPlaceId <- deref (variableValue borrowedVariable)

        if isValueContext context then do
            moveOut borrowedVariable
            id <- makeImplicitBorrowValue derefedPlaceId Const
            (mod, et) <- createPlaceExpression derefedPlaceId
            dropValue id
            return $ TypedExpression (mod $ DereferenceExpression (DereferenceExpression e')) et
        else do
            -- borrowedVariable is borrowing mutably
            return $ TypedExpression (DereferenceExpression e') (PlaceType mutability derefedPlaceId)
    typeCheck (A.UnaryExpression _ (A.Reference p) e) = do
        (e', borrowedId) <- typeCheckInPlaceContext Const e
        v <- makeBorrow borrowedId Const
        et <- createValueExpression v
        return $ TypedExpression (ReferenceExpression e') et
    typeCheck (A.UnaryExpression _ (A.ReferenceMut p) e) = do
        (e', borrowedId) <- typeCheckInPlaceContext Mutable e
        v <- makeBorrow borrowedId Mutable
        et <- createValueExpression v
        return $ TypedExpression (ReferenceExpression e') et
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
    typeCheck (A.BreakExpression p) = do
        context <- gets context
        unless (insideLoopExpression context) $ throw (BreakNotInLoop p)
        et <- makeValue p unitType False >>= createValueExpression
        return $ TypedExpression BreakExpression et
    typeCheck (A.ContinueExpression p) = do
        context <- gets context
        unless (insideLoopExpression context) $ throw (ContinueNotInLoop p)
        et <- makeValue p unitType False >>= createValueExpression
        return $ TypedExpression ContinueExpression et
    typeCheck (A.ReturnExpressionUnit p) = do
        context <- gets context
        assertType p unitType (currentFunctionReturnType context)
        et <- makeValue p unitType False >>= createValueExpression
        return $ TypedExpression (ReturnExpression (LiteralExpression VUnit)) et
    typeCheck (A.ReturnExpressionValue p e) = do
        context <- gets context
        (e', v) <- typeCheckInValueContext (Just $ currentFunctionReturnType context) e
        et <- createValueExpression v
        return $ TypedExpression (ReturnExpression e') et
    typeCheck x = do
        throw $ Other "Expression not yet implemented" (hasPosition x)

makeI32DoubleOperatorExpression :: A.BNFC'Position -> A.Expression -> A.Expression -> NumericDoubleOperator  -> PreprocessorMonad TypedExpression
makeI32DoubleOperatorExpression p e1 e2 operator = do
    (e1', v1) <- typeCheckInValueContext (Just i32Type) e1
    (e2', v2) <- typeCheckInValueContext (Just i32Type) e2
    expressionType <- makeValue p i32Type False >>= createValueExpression
    dropValue v1
    dropValue v2
    return $ TypedExpression (I32DoubleOperatorExpression p operator e1' e2') expressionType

makeComparisonOperatorExpression :: A.ComparisonOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeComparisonOperatorExpression (A.Equals p) e1 e2 = do
    (e1', id1) <- typeCheckInPlaceContext Const e1
    v1 <- makeImplicitBorrowValue id1 Const
    (e2', id2) <- typeCheckInPlaceContext Const e2
    v2 <- makeImplicitBorrowValue id2 Const

    assertType p (valueType v1) (valueType v2)

    let TReference _ innerT = valueType v1
    when (isFunction innerT) $ throw (CannotCompareFunctions innerT)

    expressionType <- makeValue p boolType False >>= createValueExpression
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

    assertType (hasPosition e1) (valueType v1) (TReference Const i32Type) -- here its a reference to int vs int
    assertType (hasPosition e2) (valueType v2) (TReference Const i32Type)
    expressionType <- makeValue (hasPosition e1) boolType False >>= createValueExpression
    dropValue v1
    dropValue v2
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
    dropValue (variableValue place)

    (e2', newValue) <- typeCheckInValueContext Nothing e2
    let t2 = valueType newValue

    assertType p t1 t2

    when (variableState place == Uninitialized) $ mutateVariableById placeId (setVariableState Free)
    mutateVariableById placeId (mutateVariableValue (const (setValueOwned True newValue)))

    et <- makeValue (hasPosition e1) unitType False >>= createValueExpression
    return $ TypedExpression (AssignmentExpression e1' e2') et

makeCompoundAssignmentExpression :: NumericDoubleOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeCompoundAssignmentExpression op e1 e2 = do
    (e2', modifyingValue) <- typeCheckInValueContext (Just i32Type) e2

    (e1', placeId) <- typeCheckInPlaceContext Mutable e1
    place <- getVariableById placeId
    let t1 = variableType place
    dropValue (variableValue place)

    assertType (hasPosition e1) t1 i32Type

    let p = hasPosition e1

    when (variableState place == Uninitialized) $ throw (UninitializedVariableUsed p placeId)

    et <- makeValue p unitType False >>= createValueExpression
    return $ TypedExpression (AssignmentExpression e1' (I32DoubleOperatorExpression p op (DereferenceExpression e1') e2')) et

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
        (condition', _) <- typeCheckInValueContext (Just boolType) condition
        return condition'

    -- need to preserve state of variables before onTrue and onFalse. Then merge them
    Variables mappings variablesBeforeIf <- gets variables
    (onTrue', onTrueValue) <- do
        (onTrue', onTrueValue) <- typeCheckInValueContext Nothing onTrue
        return (onTrue', onTrueValue)
    let onTrueType = valueType onTrueValue
    Variables _ variablesAfterTrue <- gets variables

    putVariables $ Variables mappings variablesBeforeIf
    (onFalse', onFalseValue) <- do
        if isNothing onFalse then do
            value <- makeValue p unitType False
            return (Nothing, value)
        else do
            (onFalse', onFalseValue) <- typeCheckInValueContext Nothing (fromJust onFalse)
            return (Just onFalse', onFalseValue)
    let onFalseType = valueType onFalseValue
    Variables _ variablesAfterFalse <- gets variables

    putPosition p
    mergedVariables <- mergeVariables variablesAfterTrue variablesAfterFalse
    putVariables $ Variables mappings mergedVariables

    t <- if isFunction onTrueType && isFunction onFalseType then do
        mergeFunctionTypesOrThrow (hasPosition onTrue) onTrueType onFalseType
    else do
        assertType (hasPosition onTrue) onTrueType onFalseType
        return onTrueType

    let value = Value {
        valueType = t,
        ownedPlaces = ownedPlaces onTrueValue ++ ownedPlaces onFalseValue,
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
        valueExpression <- makeValue p charType False >>= createValueExpression
        return $ TypedExpression (LiteralExpression $ VChar char) valueExpression
    typeCheck (A.LiteralString p string) = do
        valueExpression <- makeValue p (TArray charType) False >>= createValueExpression
        return $ TypedExpression (MakeArrayExpression (fmap (LiteralExpression . VChar) string)) valueExpression
    typeCheck (A.LiteralInteger p integer) = do
        valueExpression <- makeValue p i32Type False >>= createValueExpression
        return $ TypedExpression (LiteralExpression $ VI32 (fromIntegral integer)) valueExpression
    typeCheck (A.LiteralBoolean p (A.BoolTrue _)) = do
        valueExpression <- makeValue p boolType False >>= createValueExpression
        return $ TypedExpression (LiteralExpression $ VBool True) valueExpression
    typeCheck (A.LiteralBoolean p (A.BoolFalse _)) = do
        valueExpression <- makeValue p boolType False >>= createValueExpression
        return $ TypedExpression (LiteralExpression $ VBool False) valueExpression

instance TypeCheck A.ArrayElement TypedExpression where
    typeCheck :: A.ArrayElement -> PreprocessorMonad TypedExpression
    typeCheck (A.ArrayElement _ expression) = do
        typeCheck expression

instance TypeCheck A.CallParam TypedExpression where
    typeCheck :: A.CallParam -> PreprocessorMonad TypedExpression
    typeCheck (A.CallParam _ expression) = do
        typeCheck expression

instance TypeCheck A.FunctionKind FunctionKind where
    typeCheck (A.Once _) = return FnOnce
    typeCheck (A.Normal _) = return Fn

instance TypeCheck A.FunctionTypeParam Type where
    typeCheck (A.FunctionTypeParam _ t) = typeCheck t

instance TypeCheck A.Lifetime () where
    typeCheck (A.ExplicitLifetime p t) =
        throw $ Other "Explicit lifetimes not yet implemented" p
    typeCheck (A.ImplicitLifetime _) = return ()

instance TypeCheck A.FunctionTypeReturnType Type where
    typeCheck :: A.FunctionTypeReturnType -> PreprocessorMonad Type
    typeCheck (A.FunctionTypeReturnType _ t) = do
        typeCheck t
    typeCheck (A.FunctionTypeReturnTypeUnit _) = do
        return unitType

instance TypeCheck A.FunctionReturnType Type where
    typeCheck :: A.FunctionReturnType -> PreprocessorMonad Type
    typeCheck (A.ReturnValue _ t) = do
        typeCheck t
    typeCheck (A.ReturnUnit _) = do
        return unitType

typeCheckInValueContext :: (TypeCheck a TypedExpression, HasPosition a) => Maybe Type -> a -> PreprocessorMonad (Expression, Value)
typeCheckInValueContext t e = withinContext $ do
    putExpressionContext $ ValueContext t
    putPosition (hasPosition e)
    TypedExpression e' et <- typeCheck e
    let ValueType v = et
    forM_ t (assertType (hasPosition e) (valueType v))
    return (e', v)

typeCheckInPlaceContext :: (TypeCheck a TypedExpression, HasPosition a) => Mutable -> a -> PreprocessorMonad (Expression, VariableId)
typeCheckInPlaceContext mutable e = withinContext $ do
    putExpressionContext $ PlaceContext mutable
    putPosition (hasPosition e)
    TypedExpression e' et <- typeCheck e
    let PlaceType _ v = et
    return (e', v)
