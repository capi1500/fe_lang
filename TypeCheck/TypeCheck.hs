{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeCheck.TypeCheck where

import Data.Bits (xor)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Map (empty)
import Data.Foldable (traverse_)
import Control.Monad.Except
import Control.Monad.State

import qualified Fe.Abs as A
import Fe.Abs (HasPosition(hasPosition))

import Common.Ast
import Common.Utils
import Common.Types
import Common.Scope

import TypeCheck.Utils
import TypeCheck.State
import TypeCheck.StateFunctions
import TypeCheck.Error
import TypeCheck.Variable
import Data.List (intercalate)
import Common.Printer

class TypeCheck a b where
    typeCheck :: a -> PreprocessorMonad b

instance TypeCheck A.Type Type where
    typeCheck :: A.Type -> PreprocessorMonad Type
    typeCheck (A.TypeSimple p modifier ident) = do
        t <- getType (Identifier p ident)
        modifyType t modifier
    typeCheck (A.TypeArray _ modifier (A.ArrayType p t)) = do
        t' <- typeCheck t
        return $ arrayType t'
    typeCheck (A.TypeFunction p functionKind lifetime params returnType) = do
    throw $ Other "Not yet implemented" p


instance TypeCheck A.Code Code where
    typeCheck :: A.Code -> PreprocessorMonad Code
    typeCheck (A.Code p statements) = do
        traverse_ initializeGlobalScope statements
        maybeMain <- getVariable mainFunction
        statements' <- traverse (\s -> do
            updateLifetime (hasPosition s)
            typeCheck s) statements
        return $ Code statements'

initializeGlobalScope :: A.Statement -> PreprocessorMonad ()
initializeGlobalScope (A.ItemStatement _ item) = addToScope item
initializeGlobalScope _ = do return ()

addToScope :: A.Item -> PreprocessorMonad ()
addToScope (A.ItemFunction p ident lifetimes params returnType _) = do
    params' <- traverse typeCheck params
    declaredReturnType <- typeCheck returnType
    -- lifetimes' <- typeCheck lifetimes
    let identifier = Identifier p ident
    let t = TFunction
            (NamedFunction identifier)
            Fn
            params'
            declaredReturnType
    addVariable identifier Const t Free
    return ()
addToScope (A.ItemStruct p ident lifetimes fields) = do
    throw $ Other "Not yet implemented" p
addToScope (A.ItemVariant p ident lifetimes types) = do
    throw $ Other "Not yet implemented" p
addToScope (A.ItemVariable p cv ident typeDeclaration initialization) = do
    scope <- gets typeDefinitions
    when (isGlobal scope && isVariableCV cv) (do throw $ VariableAtGlobalScope (Identifier p ident))

instance TypeCheck A.Statement Statement where
    typeCheck :: A.Statement -> PreprocessorMonad Statement
    typeCheck (A.SemicolonStatement p) = do
        return EmptyStatement
    typeCheck (A.ItemStatement p item) = do
        scope <- gets typeDefinitions
        unless (isGlobal scope) $ do addToScope item
        typeCheck item
    typeCheck (A.ExpressionStatement p expression) = do
        expression' <- typeCheck expression
        return (ExpressionStatement expression')

instance TypeCheck A.Item Statement where
    typeCheck :: A.Item -> PreprocessorMonad Statement
    -- assumes function name is added to scope
    typeCheck (A.ItemFunction p ident lifetimes _ _ expression) = do
        let identifier = Identifier p ident
        (id, Variable originalIdentifier _ functionType _ _ _ lifetime) <- getVariable identifier

        let TFunction _ _ params declaredType = functionType

        let addFunctionParam = \(FunctionParam i t) -> do
                addVariable i Const t Free -- function parameters are not mutable (but if they are mutable references, they still can be written to)
                let Identifier _ ident = i
                return ident

        (expression', actualType, paramIds) <- inNewFrame p $ do
                paramIds <- traverse addFunctionParam params
                TypedExpression expression' actualType actualLifetime <- typeCheck expression
                -- TODO: for now, only static expressions (temporary and moved values) are returned
                addWarning $ Debug ("Returning value with lifetime: " ++ show actualLifetime)
                unless (isSubLifetime actualLifetime staticLifetime) $ do throw (LifetimesMismatch Nothing (hasPosition expression) staticLifetime actualLifetime)
                return (expression', actualType, paramIds)

        assertType declaredType actualType p
        return $ NewFunctionStatement ident expression' paramIds

    typeCheck (A.ItemStruct p ident lifetimes fields) = do
        throw $ Other "Not yet implemented" p
    typeCheck (A.ItemVariant p ident lifetimes subtypes) = do
        throw $ Other "Not yet implemented" p
    typeCheck (A.ItemVariable p cv ident typeDeclaration initialization) = do
        let identifier = Identifier p ident
        when (isConstCV cv && isUnInitialized initialization) $ throw $ ConstantNotInitialized identifier

        declaredType <- typeCheck typeDeclaration
        (initialization', t) <- if isInitialized initialization then do
            let A.Initialized _ expression = initialization
            putContext RValue
            TypedExpression expression' initializedType initializationLifetime <- typeCheck expression
            when (declaredType /= TUntyped) $ assertType declaredType initializedType p
            lifetime <- getLifetime
            unless (isSubLifetime initializationLifetime lifetime) $ throw (LifetimesMismatch p (hasPosition expression) lifetime initializationLifetime)
            return (VarInitialized expression', initializedType)
        else do
            return (VarUninitialized, declaredType)

        let variableState =
                if isUnInitialized initialization then Uninitialized
                else Free
        
        let mutability = if isConstCV cv then Const else Mutable
        id <- addVariable identifier mutability t variableState
        handleUsedVariables (transferOwnership id)
        printVariables
        return $ NewVariableStatement ident initialization'

instance TypeCheck A.FunctionParam FunctionParam where
    typeCheck :: A.FunctionParam -> PreprocessorMonad FunctionParam
    typeCheck (A.Parameter p ident t) = do
        t' <- typeCheck t
        return $ FunctionParam (Identifier p ident) t'

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

-- each expression should handle its used variables and return (marked as used) at most one temporary variable, which holds necessary borrows
instance TypeCheck A.Expression TypedExpression where
    typeCheck :: A.Expression -> PreprocessorMonad TypedExpression
    typeCheck (A.BlockExpression p statements) = do
        statements' <- inNewScope p $ traverse typeCheck statements
        let (t, actualLifetime, lastPosition) = if null statements' then
                (unitType, staticLifetime, Nothing)
            else
                case last statements' of
                ExpressionStatement (TypedExpression _ t l) -> (t, l, hasPosition (last statements))
                _ -> (unitType, staticLifetime, Nothing)
        
        currentLifetime <- getLifetime
        unless (isSubLifetime actualLifetime currentLifetime) $ do throw (LifetimesMismatch Nothing lastPosition currentLifetime actualLifetime)
        
        return $ TypedExpression (BlockExpression statements') t actualLifetime
    typeCheck (A.GroupedExpression _ expression) = do
        typeCheck expression
    typeCheck (A.VariableExpression p ident) = do
        (id, variable) <- getVariable (Identifier p ident)
        when (variableState variable == Moved) $ throw (UseAfterMoved p id)
        let t = variableType variable
        whenContext 
            (do
                let mutability = variableMutability variable
                let t' = TReference mutability t
                referenceToVariable <- addTemporaryVariable p Const t' -- cannot write to tempotary values, but can change reference under it, if its mutable
                if isConst mutability then do
                    borrowVariable p referenceToVariable id
                else do
                    borrowMutVariable p referenceToVariable id
                markVariableUsed referenceToVariable
                printUsedVariables "LValue variable: "
                printVariables
                return $ TypedExpression (ReferenceExpression ident) t' (lifetime variable))
            (do
                markVariableUsed id
                printUsedVariables "RValue variable: "
                printVariables
                return $ TypedExpression (VariableExpression ident) t staticLifetime)
    typeCheck (A.IfExpression p ifExpression) = do
        typeCheck ifExpression
    typeCheck (A.CallExpression p function params) = do
        putContext LValue
        TypedExpression function' function_reference _ <- typeCheck function
        let TReference _ t = function_reference
        unless (isFunction t) $ do throw (ExpressionNotCallable p t)
        handleUsedVariables moveOutVariable

        let TFunction _ kind declaredParams returnType = t
        when (length params /= length declaredParams) $ throw (WrongNumberOfParams p t)
        let paramCheck = \(FunctionParam paramIdent paramDeclaredType, param) -> do
            putContext RValue
            TypedExpression paramExpression paramType _ <- typeCheck param
            assertType paramType paramDeclaredType (hasPosition param)
            handleUsedVariables moveOutVariable
            let Identifier _ ident = paramIdent
            return (ident, paramExpression)
        params' <- traverse paramCheck (zip declaredParams params)
        -- TODO: for now, only static expressions (temporary and moved values) are returned
        return $ TypedExpression (CallExpression function' params') returnType staticLifetime
    typeCheck (A.UnaryExpression _ (A.UnaryMinus p) e) = do
        whenLValue $ throw (IllegalInLValue p)
        TypedExpression e' t l <- typeCheck e
        assertType t i32Type (hasPosition e)
        handleUsedVariables moveOutVariable
        return $ TypedExpression (UnaryMinusExpression e') i32Type staticLifetime
    typeCheck (A.UnaryExpression _ (A.UnaryNegation p) e) = do
        whenLValue $ throw (IllegalInLValue p)
        TypedExpression e' t l <- typeCheck e
        assertType t boolType (hasPosition e)
        handleUsedVariables moveOutVariable
        return $ TypedExpression (UnaryNegationExpression e') boolType staticLifetime
    -- typeCheck (A.UnaryExpression _ (A.Dereference p) e) = do
    typeCheck (A.UnaryExpression _ (A.Reference p) e) = do
        whenLValue $ throw (IllegalInLValue p)
        putContext LValue
        TypedExpression e' t l <- typeCheck e
        (derefedVariableId, derefedVariable) <- deref
        let t' = TReference Const (variableType derefedVariable)
        referenceTempVariableId <- addTemporaryVariable p Const t'
        borrowVariable p referenceTempVariableId derefedVariableId
        markVariableUsed referenceTempVariableId
        printUsedVariables "Ref: "
        printVariables
        return $ TypedExpression e' t' l
    typeCheck (A.UnaryExpression _ (A.ReferenceMut p) e) = do
        whenLValue $ throw (IllegalInLValue p)
        putContext LValue
        TypedExpression e' t l <- typeCheck e
        (derefedVariableId, derefedVariable) <- deref
        let t' = TReference Mutable (variableType derefedVariable)
        referenceTempVariableId <- addTemporaryVariable p Const t'
        borrowMutVariable p referenceTempVariableId derefedVariableId
        markVariableUsed referenceTempVariableId
        printUsedVariables "Ref mut: "
        printVariables
        return $ TypedExpression e' t' l
    typeCheck (A.LiteralExpression p literal) = do
        whenLValue $ throw (IllegalInLValue p)
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
        whenLValue $ throw (IllegalInLValue p)
        makeComparisonOperatorExpression operator e1 e2
    typeCheck (A.AssignmentExpression _ e1 operator e2) = do
        makeAssignmentOperatorExpression operator e1 e2
    typeCheck x = do
        throw $ Other "Not yet implemented" (hasPosition x)

makeI32DoubleOperatorExpression :: A.BNFC'Position -> A.Expression -> A.Expression -> NumericDoubleOperator  -> PreprocessorMonad TypedExpression
makeI32DoubleOperatorExpression p e1 e2 operator = do
    whenLValue $ throw (IllegalInLValue p)
    TypedExpression e1' t1 _ <- typeCheck e1
    handleUsedVariables moveOutVariable
    putContext RValue
    TypedExpression e2' t2 _ <- typeCheck e2
    handleUsedVariables moveOutVariable
    assertType t1 i32Type (hasPosition e1)
    assertType t2 i32Type (hasPosition e2)
    handleUsedVariables moveOutVariable
    return $ TypedExpression (I32DoubleOperatorExpression operator e1' e2') i32Type staticLifetime

makeComparisonOperatorExpression :: A.ComparisonOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeComparisonOperatorExpression (A.Equals p) e1 e2 = do
    TypedExpression e1' t1 _ <- typeCheck e1
    handleUsedVariables moveOutVariable
    putContext RValue
    TypedExpression e2' t2 _ <- typeCheck e2
    handleUsedVariables moveOutVariable
    assertType t1 t2 p
    return $ TypedExpression (BoolDoubleOperatorExpression Equals e1' e2') boolType staticLifetime
makeComparisonOperatorExpression (A.NotEquals p) e1 e2 = do
    TypedExpression e1' t1 _ <- typeCheck e1
    handleUsedVariables moveOutVariable
    putContext RValue
    TypedExpression e2' t2 _ <- typeCheck e2
    handleUsedVariables moveOutVariable
    assertType t1 t2 p
    return $ TypedExpression (UnaryNegationExpression (BoolDoubleOperatorExpression Equals e1' e2')) boolType staticLifetime
makeComparisonOperatorExpression (A.Greater p) e1 e2 = do
    makeI32ComparisonExpression e1 e2 Greater
makeComparisonOperatorExpression (A.Smaller p) e1 e2 = do
    makeI32ComparisonExpression e1 e2 Smaller
makeComparisonOperatorExpression (A.GreaterEquals p) e1 e2 = do
    makeI32ComparisonExpression e1 e2 GreaterEquals
makeComparisonOperatorExpression (A.SmallerEquals p) e1 e2 = do
    makeI32ComparisonExpression e1 e2 SmallerEquals

makeI32ComparisonExpression :: A.Expression -> A.Expression -> BooleanDoubleOperator -> PreprocessorMonad TypedExpression
makeI32ComparisonExpression e1 e2 operator = do
    TypedExpression e1' t1 _ <- typeCheck e1
    handleUsedVariables moveOutVariable
    putContext RValue
    TypedExpression e2' t2 _ <- typeCheck e2
    handleUsedVariables moveOutVariable
    assertType t1 i32Type (hasPosition e1)
    assertType t2 i32Type (hasPosition e2)
    return $ TypedExpression (BoolDoubleOperatorExpression operator e1' e2') boolType staticLifetime

makeAssignmentOperatorExpression :: A.AssignmentOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeAssignmentOperatorExpression (A.PlusEqual p) e1 e2 = do makeAssignmentOperatorExpression' Plus e1 e2
makeAssignmentOperatorExpression (A.MinusEqual p) e1 e2 = do makeAssignmentOperatorExpression' Minus e1 e2
makeAssignmentOperatorExpression (A.MultiplyEqual p) e1 e2 = do makeAssignmentOperatorExpression' Multiply e1 e2
makeAssignmentOperatorExpression (A.DivideEqual p) e1 e2 = do makeAssignmentOperatorExpression' Divide e1 e2
makeAssignmentOperatorExpression (A.ModuloEqual p) e1 e2 = do makeAssignmentOperatorExpression' Modulo e1 e2
makeAssignmentOperatorExpression (A.AndEqual p) e1 e2 = do makeAssignmentOperatorExpression' BitAnd e1 e2
makeAssignmentOperatorExpression (A.OrEqual p) e1 e2 = do makeAssignmentOperatorExpression' BitOr e1 e2
makeAssignmentOperatorExpression (A.XorEqual p) e1 e2 = do makeAssignmentOperatorExpression' BitXor e1 e2
makeAssignmentOperatorExpression (A.LShiftEqual p) e1 e2 = do makeAssignmentOperatorExpression' LShift e1 e2
makeAssignmentOperatorExpression (A.RShiftEqual p) e1 e2 = do makeAssignmentOperatorExpression' RShift e1 e2
makeAssignmentOperatorExpression (A.Assign p) e1 e2 = do
    putContext LValue
    TypedExpression e1' _ l1 <- typeCheck e1
    (assignedVariableId, assignedVariable) <- deref
    
    let state = variableState assignedVariable
    let t1 = variableType assignedVariable

    when (variableMutability assignedVariable == Const) $ throw (AssignmentToConstant p assignedVariableId)
    printUsedVariables "Assignment left: "
    printVariables
    when (state == Free) $ moveOutVariable assignedVariableId
    
    putContext RValue
    TypedExpression e2' t2 l2 <- typeCheck e2
    assertType t1 t2 p

    printUsedVariables "Assignment Right: "
    handleUsedVariables (transferOwnership assignedVariableId)
    mutateVariableById assignedVariableId (setVariableState Free)
    printVariables

    return $ TypedExpression (AssignmentExpression (isReference t1) e1' e2') unitType staticLifetime

makeAssignmentOperatorExpression' :: NumericDoubleOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeAssignmentOperatorExpression' op e1 e2 = do
    throw $ Other "Not yet implemented" (hasPosition e1)
    -- TypedExpression e1' t _ <- typeCheck e1
    -- TypedExpression e2' t _ <- typeCheck e2
    -- return $ TypedExpression (AssignmentExpression False e1' (I32DoubleOperatorExpression op e1' e2')) unitType staticLifetime

instance TypeCheck A.IfExpression TypedExpression where
    typeCheck :: A.IfExpression -> PreprocessorMonad TypedExpression
    typeCheck (A.If p condition onTrue) =
        ifExpression p condition onTrue (A.BlockExpression p [])
    typeCheck (A.IfElse p condition onTrue onFalse) =
        ifExpression p condition onTrue onFalse
    typeCheck (A.IfElseIf p condition onTrue onFalse) =
        ifExpression p condition onTrue onFalse

ifExpression :: (TypeCheck a TypedExpression, HasPosition a) => A.BNFC'Position -> A.Expression -> A.Expression -> a -> PreprocessorMonad TypedExpression
ifExpression p condition onTrue onFalse = do
    putContext RValue
    TypedExpression condition' conditionType _ <- typeCheck condition
    assertType conditionType boolType (hasPosition condition)
    handleUsedVariables moveOutVariable
    
    TypedExpression onTrue' onTrueType onTrueLifetime <- typeCheck onTrue
    printUsedVariables "ifExpression, onTrue: "
    onTrueUsed <- gets usedVariables
    clearUsedVariables
    
    TypedExpression onFalse' onFalseType onFalseLifetime <- typeCheck onFalse
    printUsedVariables "ifExpression, onFalse: "
    onFalseUsed <- gets usedVariables
    clearUsedVariables

    t <- if isFunction onTrueType && isFunction onFalseType then do
        mergeFunctionTypesOrThrow (hasPosition onTrue) onTrueType onFalseType
    else do
        assertType onFalseType onTrueType (hasPosition onTrue)
        return onTrueType

    when (isJust onTrueUsed || isJust onFalseUsed) $ do
        mergedTmpVariable <- addTemporaryVariable p Const t
        when (isJust onTrueUsed) $ do transferOwnership mergedTmpVariable (fromJust onTrueUsed)
        when (isJust onFalseUsed) $ do transferOwnership mergedTmpVariable (fromJust onFalseUsed)
        markVariableUsed mergedTmpVariable

    lifetime <- getShorterOfLifetimesOrThrow (hasPosition onTrue) (hasPosition onFalse) onTrueLifetime onFalseLifetime
    return $ TypedExpression (IfExpression condition' onTrue' (Just onFalse')) t lifetime

instance TypeCheck A.Literal TypedExpression where
    typeCheck :: A.Literal -> PreprocessorMonad TypedExpression
    typeCheck (A.LiteralChar p char) = do
        return $ TypedExpression (LiteralExpression $ VChar char) charType staticLifetime
    typeCheck (A.LiteralString p string) = do
        return $ TypedExpression (MakeArrayExpression (fmap VChar string)) stringType staticLifetime
    typeCheck (A.LiteralInteger p integer) = do
        return $ TypedExpression (LiteralExpression $ VI32 (fromIntegral integer)) i32Type staticLifetime
    typeCheck (A.LiteralBoolean p (A.BoolTrue _)) = do
        return $ TypedExpression (LiteralExpression $ VBool True) boolType staticLifetime
    typeCheck (A.LiteralBoolean p (A.BoolFalse _)) = do
        return $ TypedExpression (LiteralExpression $ VBool False) boolType staticLifetime

instance TypeCheck A.CallParam TypedExpression where
    typeCheck :: A.CallParam -> PreprocessorMonad TypedExpression
    typeCheck (A.CallParam _ expression) = do
        typeCheck expression
