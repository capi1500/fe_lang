{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeCheck.TypeCheck where

import Data.Bits (xor)
import Data.Maybe (isNothing, isJust)
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
    addVariable identifier t Uninitialized
    return ()
addToScope (A.ItemStruct p ident lifetimes fields) = do
    throw $ Other "Not yet implemented" p
addToScope (A.ItemVariant p ident lifetimes types) = do
    throw $ Other "Not yet implemented" p
addToScope (A.ItemVariable p cv ident typeDeclaration initialization) = do
    scope <- gets typeDefinitions
    when (isGlobal scope && isVariable cv) (do throw $ VariableAtGlobalScope (Identifier p ident))

instance TypeCheck A.Statement Statement where
    typeCheck :: A.Statement -> PreprocessorMonad Statement
    typeCheck (A.SemicolonStatement p) = do
        return EmptyStatement
    typeCheck (A.ItemStatement p item) = do
        addToScope item
        Variables _ variables <- gets variables
        addWarning $ Debug ("Added item " ++ nameOfItem item ++ ": [\n" ++ intercalate ",\n" (fmap (codePrint 2) variables) ++ "]")
        typeCheck item
    typeCheck (A.ExpressionStatement p expression) = do
        expression' <- typeCheck expression
        usedVariables <- gets usedVariables
        handleUsedVariables moveOutVariable
        return (ExpressionStatement expression')

instance TypeCheck A.Item Statement where
    typeCheck :: A.Item -> PreprocessorMonad Statement
    -- assumes function name is added to scope
    typeCheck (A.ItemFunction p ident lifetimes _ _ expression) = do
        let identifier = Identifier p ident
        (id, Variable originalIdentifier functionType _ _ _ lifetime) <- getVariable identifier

        let TFunction _ _ params declaredType = functionType

        let addFunctionParam = \(FunctionParam i t) -> do
                addVariable i t Free
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
        id <- addVariable identifier functionType Free
        return $ NewFunctionStatement ident expression' paramIds

    typeCheck (A.ItemStruct p ident lifetimes fields) = do
        throw $ Other "Not yet implemented" p
    typeCheck (A.ItemVariant p ident lifetimes subtypes) = do
        throw $ Other "Not yet implemented" p
    typeCheck (A.ItemVariable p cv ident typeDeclaration initialization) = do
        let identifier = Identifier p ident
        when (isConst cv && isUnInitialized initialization) $ throw $ ConstantNotInitialized identifier

        declaredType <- typeCheck typeDeclaration
        (initialization', t) <- if isInitialized initialization then do
            let A.Initialized _ expression = initialization
            TypedExpression expression' initializedType initializationLifetime <- typeCheck expression
            when (declaredType /= TUntyped) $ assertType declaredType initializedType p
            lifetime <- getLifetime
            unless (isSubLifetime initializationLifetime lifetime) $ throw (LifetimesMismatch p (hasPosition expression) lifetime initializationLifetime)
            return (VarInitialized expression', initializedType)
        else do
            return (VarUninitialized, declaredType)

        when (isVariable cv && isConstReference t) $ throw (InitializeConstantAsMutable identifier t)
        let variableState =
                if isUnInitialized initialization then Uninitialized
                else Free

        id <- addVariable identifier t variableState
        handleUsedVariables (transferOwnership id)
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

-- each expression should handle its used variables
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
        -- TODO: this should not restrict to the static lifetime, see multiple_variables_on_write as a counterexample
        unless (isSubLifetime actualLifetime staticLifetime) $ do throw (LifetimesMismatch Nothing lastPosition staticLifetime actualLifetime)
        return $ TypedExpression (BlockExpression statements') t actualLifetime
    typeCheck (A.GroupedExpression _ expression) = do
        typeCheck expression
    typeCheck (A.VariableExpression p ident) = do
        (id, variable) <- getVariable (Identifier p ident)
        markVariableUsed id
        return $ TypedExpression (VariableExpression ident) (variableType variable) staticLifetime
    typeCheck (A.IfExpression p ifExpression) = do
        typeCheck ifExpression
    typeCheck (A.CallExpression p function params) = do
        TypedExpression function' t _ <- typeCheck function
        unless (isFunction t) $ do throw (ExpressionNotCallable p t)
        handleUsedVariables moveOutVariable

        let TFunction _ kind declaredParams returnType = t
        when (length params /= length declaredParams) $ throw (WrongNumberOfParams p t)
        let paramCheck = \(FunctionParam paramIdent paramDeclaredType, param) -> do
            TypedExpression paramExpression paramType _ <- typeCheck param
            assertType paramType paramDeclaredType (hasPosition param)
            handleUsedVariables moveOutVariable
            let Identifier _ ident = paramIdent
            return (ident, paramExpression)
        params' <- traverse paramCheck (zip declaredParams params)
        -- TODO: for now, only static expressions (temporary and moved values) are returned
        return $ TypedExpression (CallExpression function' params') returnType staticLifetime
    typeCheck (A.UnaryExpression _ (A.UnaryMinus p) e) = do
        TypedExpression e' t l <- typeCheck e
        assertType t i32Type (hasPosition e)
        handleUsedVariables moveOutVariable
        return $ TypedExpression (UnaryMinusExpression e') i32Type staticLifetime
    typeCheck (A.UnaryExpression _ (A.UnaryNegation p) e) = do
        TypedExpression e' t l <- typeCheck e
        assertType t boolType (hasPosition e)
        handleUsedVariables moveOutVariable
        return $ TypedExpression (UnaryNegationExpression e') boolType staticLifetime
    -- typeCheck (A.UnaryExpression _ (A.Dereference p) e) = do
    --     throw $ Other "Not yet implemented" (hasPosition x)
    typeCheck (A.UnaryExpression _ (A.Reference p) e) = do
        TypedExpression e' t _ <- typeCheck e
        let t' = TReference Const t
        referenceTempVariableId <- addTemporaryVariable p t'
        lifetime <- getShortestLifetimeOfUsedVariables p
        handleUsedVariables (borrowVariable referenceTempVariableId)
        markVariableUsed referenceTempVariableId
        return $ TypedExpression e' t' lifetime
    typeCheck (A.UnaryExpression _ (A.ReferenceMut p) e) = do
        TypedExpression e' t _ <- typeCheck e
        let t' = TReference Mutable t
        referenceTempVariableId <- addTemporaryVariable p t'
        lifetime <- getShortestLifetimeOfUsedVariables p
        handleUsedVariables (borrowMutVariable referenceTempVariableId)
        markVariableUsed referenceTempVariableId
        return $ TypedExpression e' t' lifetime
    typeCheck (A.LiteralExpression _ literal) = do
        typeCheck literal
    typeCheck (A.PlusExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 Plus p
    typeCheck (A.MinusExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 Minus p
    typeCheck (A.MultiplyExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 Multiply p
    typeCheck (A.DivideExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 Divide p
    typeCheck (A.ModuloExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 Modulo p
    typeCheck (A.LShiftExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 LShift p
    typeCheck (A.RShiftExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 RShift p
    typeCheck (A.BitOrExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 BitOr p
    typeCheck (A.BitXOrExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 BitXor p
    typeCheck (A.BitAndExpression p e1 e2) = do
        makeI32DoubleOperatorExpression e1 e2 BitAnd p
    typeCheck (A.ComparisonExpression _ e1 operator e2) = do
        e <- makeComparisonOperatorExpression operator e1 e2
        handleUsedVariables moveOutVariable
        return e
    typeCheck x = do
        throw $ Other "Not yet implemented" (hasPosition x)

makeI32DoubleOperatorExpression :: A.Expression -> A.Expression -> NumericDoubleOperator -> A.BNFC'Position -> PreprocessorMonad TypedExpression
makeI32DoubleOperatorExpression e1 e2 operator p = do
    TypedExpression e1' t1 _ <- typeCheck e1
    TypedExpression e2' t2 _ <- typeCheck e2
    assertType t1 i32Type (hasPosition e1)
    assertType t2 i32Type (hasPosition e2)
    handleUsedVariables moveOutVariable
    return $ TypedExpression (I32DoubleOperatorExpression operator e1' e2') i32Type staticLifetime

makeComparisonOperatorExpression :: A.ComparisonOperator -> A.Expression -> A.Expression -> PreprocessorMonad TypedExpression
makeComparisonOperatorExpression (A.Equals p) e1 e2 = do
    TypedExpression e1' t1 _ <- typeCheck e1
    TypedExpression e2' t2 _ <- typeCheck e2
    assertType t1 t2 p
    return $ TypedExpression (BoolDoubleOperatorExpression Equals e1' e2') boolType staticLifetime
makeComparisonOperatorExpression (A.NotEquals p) e1 e2 = do
    TypedExpression e1' t1 _ <- typeCheck e1
    TypedExpression e2' t2 _ <- typeCheck e2
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
    TypedExpression e2' t2 _ <- typeCheck e2
    assertType t1 i32Type (hasPosition e1)
    assertType t2 i32Type (hasPosition e2)
    return $ TypedExpression (BoolDoubleOperatorExpression operator e1' e2') boolType staticLifetime

instance TypeCheck A.IfExpression TypedExpression where
    typeCheck :: A.IfExpression -> PreprocessorMonad TypedExpression
    typeCheck (A.If p condition onTrue) =
        helper condition onTrue (A.BlockExpression p [])
    typeCheck (A.IfElse p condition onTrue onFalse) =
        helper condition onTrue onFalse
    typeCheck (A.IfElseIf p condition onTrue onFalse) =
        helper condition onTrue onFalse

helper :: (TypeCheck a TypedExpression, HasPosition a) => A.Expression -> A.Expression -> a -> PreprocessorMonad TypedExpression
helper condition onTrue onFalse = do
    TypedExpression condition' conditionType _ <- typeCheck condition
    assertType conditionType boolType (hasPosition condition)
    handleUsedVariables moveOutVariable
    TypedExpression onTrue' onTrueType onTrueLifetime <- typeCheck onTrue -- Block expression, it handles borrow checker automatically
    TypedExpression onFalse' onFalseType onFalseLifetime <- typeCheck onFalse -- Block expression, it handles borrow checker automatically
    t <- if isFunction onTrueType && isFunction onFalseType then do
        mergeFunctionTypesOrThrow (hasPosition onTrue) onTrueType onFalseType
    else do
        assertType onFalseType onTrueType (hasPosition onTrue)
        return onTrueType
    lifetime <- getShorterOfLifetimesOrThrow (hasPosition onTrue) (hasPosition onFalse) onTrueLifetime onFalseLifetime
    return $ TypedExpression (IfExpression condition' onTrue' (Just onFalse')) t lifetime

instance TypeCheck A.Literal TypedExpression where
    typeCheck :: A.Literal -> PreprocessorMonad TypedExpression
    typeCheck (A.LiteralChar p char) = do
        -- id <- addTemporaryVariable p charType
        -- markVariableUsed id
        return $ TypedExpression (LiteralExpression $ VChar char) charType staticLifetime
    typeCheck (A.LiteralString p string) = do
        -- id <- addTemporaryVariable p stringType
        -- markVariableUsed id
        return $ TypedExpression (MakeArrayExpression (fmap VChar string)) stringType staticLifetime
    typeCheck (A.LiteralInteger p integer) = do
        -- id <- addTemporaryVariable p i32Type
        -- markVariableUsed id
        return $ TypedExpression (LiteralExpression $ VI32 (fromIntegral integer)) i32Type staticLifetime
    typeCheck (A.LiteralBoolean p (A.BoolTrue _)) = do
        -- id <- addTemporaryVariable p boolType
        -- markVariableUsed id
        return $ TypedExpression (LiteralExpression $ VBool True) boolType staticLifetime
    typeCheck (A.LiteralBoolean p (A.BoolFalse _)) = do
        -- id <- addTemporaryVariable p boolType
        -- markVariableUsed id
        return $ TypedExpression (LiteralExpression $ VBool False) boolType staticLifetime

instance TypeCheck A.CallParam TypedExpression where
    typeCheck :: A.CallParam -> PreprocessorMonad TypedExpression
    typeCheck (A.CallParam _ expression) = do
        typeCheck expression
