{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module TypeCheck.TypeCheck where

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
import Common.Annotation
import TypeCheck.Utils
import TypeCheck.State
import TypeCheck.Error
import Data.Bits (xor)
import Common.Scope

class TypeCheck a b where
    typeCheck :: a -> PreprocessorMonad b


instance TypeCheck A.Type Type where
    typeCheck :: A.Type -> PreprocessorMonad Type
    typeCheck (A.TypeSimple p modifier ident) = do
        t <- getType (Identifier p ident)
        let t' = modifyType modifier t
        return t'
    typeCheck (A.TypeArray _ modifier (A.ArrayType p t)) = do
        t' <- typeCheck t
        return $ TArray $ Array t' UnSized
    typeCheck (A.TypeArray _ modifier (A.ArrayTypeSized p t size)) = do
        t' <- typeCheck t
        TypedExpression size' sizeType <- typeCheck size
        assertType sizeType i32Type p
        return $ TArray $ Array t' UnSized
    typeCheck (A.TypeFunction p functionKind lifetime params returnType) = do
        return TUntyped


instance TypeCheck A.Code Code where
    typeCheck :: A.Code -> PreprocessorMonad Code
    typeCheck (A.Code p statements) = do -- TODO: add checking for main function
        traverse_ initializeGlobalScope statements
        statements' <- traverse typeCheck statements
        get
        return $ Code statements'

initializeGlobalScope :: A.Statement -> PreprocessorMonad ()
initializeGlobalScope (A.ItemStatement _ item) = addToScope item
initializeGlobalScope _ = do return ()

addToScope :: A.Item -> PreprocessorMonad ()
addToScope (A.ItemFunction p ident params returnType _) = do
    params' <- traverse typeCheck params
    declaredReturnType <- typeCheck returnType
    let identifier = Identifier p ident
    let t = TFunction $ Function
            (NamedFunction identifier)
            Fn
            Static
            (fmap (\(FunctionParam _ t) -> t) params')
            declaredReturnType
    let f = Variable identifier t Uninitialized
    maybeVar <- tryGetVariableLocal identifier
    when (isJust maybeVar) $ do
        let Just (id, Variable originalIdentifier _ state) = maybeVar
        unless (state == Uninitialized) $ addWarning (Shadow originalIdentifier identifier)
        free id
    id <- addVariable identifier f
    return ()
addToScope (A.ItemStruct p ident fields) = do
    throwError $ Other "Not yet implemented" p
addToScope (A.ItemVariant p ident types) = do
    throwError $ Other "Not yet implemented" p
addToScope (A.ItemVariable p cv ident typeDeclaration initialization) = do
    PreprocessorState scope _ _ <- get
    when (isGlobal scope && isVariable cv) (do throwError $ VariableAtGlobalScope (Identifier p ident))

    -- typeCheck (A.ItemVariable p cv ident typeDeclaration initialization) :: PreprocessorMonad Statement

    -- return ()

instance TypeCheck A.Statement Statement where
    typeCheck :: A.Statement -> PreprocessorMonad Statement
    typeCheck (A.SemicolonStatement p) = do
        return EmptyStatement
    typeCheck (A.ItemStatement p item) = do
        addToScope item
        typeCheck item
    typeCheck (A.ExpressionStatement p expression) = do
        expression' <- typeCheck expression
        return $ ExpressionStatement expression'

instance TypeCheck A.Item Statement where
  typeCheck :: A.Item -> PreprocessorMonad Statement
  typeCheck (A.ItemFunction p ident params returnType expression) = do
    let identifier = Identifier p ident
    (id, Variable originalIdentifier functionType state) <- getVariable identifier

    let (TFunction (Function _ _ _ _ declaredType)) = functionType

    TypedExpression expression' actualType <- typeCheck expression
    assertType declaredType actualType p

    let f = Variable identifier actualType Free
    free id
    id <- addVariable identifier f

    return $ NewVariableStatement identifier id actualType (VarInitialized expression')

  typeCheck (A.ItemStruct p ident fields) = do
    throwError $ Other "Not yet implemented" p
  typeCheck (A.ItemVariant p ident subtypes) = do
    throwError $ Other "Not yet implemented" p
  typeCheck (A.ItemVariable p cv ident typeDeclaration initialization) = do
    let identifier = Identifier p ident
    when (isConst cv && isUnInitialized initialization) $ throwError $ ConstantNotInitialized identifier

    declaredType <- typeCheck typeDeclaration
    (initialization', t) <- if isInitialized initialization then do
        let A.Initialized _ expression = initialization
        TypedExpression expression' initializedType <- typeCheck expression
        when (declaredType /= TUntyped) $ assertType declaredType initializedType p

        return (VarInitialized expression', initializedType)
    else do
        return (VarUninitialized, declaredType)

    let variableState =
            if isUnInitialized initialization
                then
                    Uninitialized
                else
                    Free

    let v = Variable
            identifier
            t
            variableState
    id <- addVariable identifier v
    return $ NewVariableStatement identifier id t initialization'


-- instance TypeCheck A.Function' where
--   typeCheck :: A.Function' A.BNFC'Position -> PreprocessorMonad (A.Function' Annotation)
--   typeCheck (A.Function p ident params returnType code) = do
--     params' <- traverse typeCheck params
--     code' <- typeCheck code
--     returnType' <- typeCheck returnType
--     let t = TFunction $ Function
--             (NamedFunction (Identifier p ident))
--             Fn
--             Static
--             (fmap getAnnotatedType params')
--             (getAnnotatedType returnType')
--     return $ A.Function (Typed p t) ident params' returnType' code'

data FunctionParam = FunctionParam Identifier Type

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
    return $ TPrimitive Unit

-- instance TypeCheck A.Struct' where
--   typeCheck :: A.Struct' A.BNFC'Position -> PreprocessorMonad (A.Struct' Annotation)
--   typeCheck (A.Struct p ident fields) = do
--     fields' <- traverse typeCheck fields
--     let identifier = Identifier p ident
--     let mapper (A.StructField _ ident t) = Field identifier (getAnnotatedType t)
--     let t = TStruct $ Struct identifier (fmap mapper fields')
--     return $ A.Struct (Typed p t) ident fields'

-- instance TypeCheck A.StructField' where
--   typeCheck :: A.StructField' A.BNFC'Position -> PreprocessorMonad (A.StructField' Annotation)
--   typeCheck (A.StructField p ident t) = do
--     t' <- typeCheck t -- allowing all, even unsized arrays and self recursion
--     return $ A.StructField (Typed p (getAnnotatedType t')) ident t'

-- instance TypeCheck A.Variant' where
--   typeCheck :: A.Variant' A.BNFC'Position -> PreprocessorMonad (A.Variant' Annotation)
--   typeCheck (A.Variant p ident items) = do
--     items' <- traverse typeCheck items
--     let t = TVariant $ Variant (Identifier p ident) (fmap getAnnotatedType items')
--     return $ A.Variant (Typed p t) ident items'

-- instance TypeCheck A.VariantSubtype' where
--   typeCheck :: A.VariantSubtype' A.BNFC'Position -> PreprocessorMonad (A.VariantSubtype' Annotation)
--   typeCheck (A.VariantSubtype p t) = do
--     t' <- typeCheck t -- allowing all, even unsized arrays and self recursion
--     return $ A.VariantSubtype (Typed p (getAnnotatedType t')) t'

instance TypeCheck A.TypeDeclaration Type where
  typeCheck :: A.TypeDeclaration -> PreprocessorMonad Type
  typeCheck (A.Untyped _) = do
    return TUntyped
  typeCheck (A.Typed _ t) = do
    typeCheck t

-- instance TypeCheck A.FunctionType' where
--   typeCheck :: A.FunctionType' A.BNFC'Position -> PreprocessorMonad (A.FunctionType' Annotation)
--   typeCheck (A.FunctionType p functionKind lifetime parameters returnType) = do
--     functionKind' <- typeCheck functionKind
--     lifetime' <- typeCheck lifetime
--     parameters' <- traverse typeCheck parameters
--     returnType' <- typeCheck returnType
--     let t = TFunction $ Function
--             Unnamed
--             (case functionKind' of
--             A.Once _ -> FnOnce
--             A.Normal _ -> Fn)
--             Static
--             (fmap getAnnotatedType parameters') (getAnnotatedType returnType')
--     return $ A.FunctionType (Typed p t) functionKind' lifetime' parameters' returnType'

-- instance TypeCheck A.FunctionKind'

-- instance TypeCheck A.FunctionTypeParam' where
--   typeCheck :: A.FunctionTypeParam' A.BNFC'Position -> PreprocessorMonad (A.FunctionTypeParam' Annotation)
--   typeCheck (A.FunctionTypeParam p t) = do
--     t' <- typeCheck t
--     return $ A.FunctionTypeParam (Typed p (getAnnotatedType t')) t'

-- instance TypeCheck A.FunctionTypeReturnType' where
--   typeCheck :: A.FunctionTypeReturnType' A.BNFC'Position -> PreprocessorMonad (A.FunctionTypeReturnType' Annotation)
--   typeCheck (A.FunctionTypeReturnType p t) = do
--     t' <- typeCheck t
--     return $ A.FunctionTypeReturnType (Typed p (TPrimitive Unit)) t'
--   typeCheck (A.FunctionTypeReturnTypeUnit p) = do
--     return $ A.FunctionTypeReturnTypeUnit (Typed p (TPrimitive Unit))

instance TypeCheck A.Expression TypedExpression where
  typeCheck :: A.Expression -> PreprocessorMonad TypedExpression
  typeCheck (A.BlockExpression _ statements) = do
    statements' <- inNewScope $ traverse typeCheck statements
    return $ TypedExpression (BlockExpression statements') (typeOfBlock statements')
  typeCheck (A.GroupedExpression _ expression) = do
    typeCheck expression
  typeCheck (A.VariableExpression p ident) = do
    (id, Variable identifier t state) <- getVariable (Identifier p ident)
    when (state == Uninitialized) $ throwError (UninitializedVariableUsed p identifier)
    -- TODO: deal with borrow checker
    return $ TypedExpression (VariableExpression id) t
  typeCheck (A.LiteralExpression p literal) = do
    typeCheck literal
  typeCheck (A.PlusExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 Plus
  typeCheck (A.MinusExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 Minus
  typeCheck (A.MultiplyExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 Multiply
  typeCheck (A.DivideExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 Divide
  typeCheck (A.ModuloExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 Modulo
  typeCheck (A.LShiftExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 LShift
  typeCheck (A.RShiftExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 RShift
  typeCheck (A.BitOrExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 BitOr
  typeCheck (A.BitXOrExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 BitXor
  typeCheck (A.BitAndExpression _ e1 e2) = do
    makeI32DoubleOperatorExpression e1 e2 BitAnd
  typeCheck x = do
    throwError $ Other "Not yet implemented" (hasPosition x)

makeI32DoubleOperatorExpression :: A.Expression -> A.Expression -> NumericDoubleOperator -> PreprocessorMonad TypedExpression
makeI32DoubleOperatorExpression e1 e2 operator = do
    TypedExpression e1' t1 <- typeCheck e1
    TypedExpression e2' t2 <- typeCheck e2
    assertType t1 i32Type (hasPosition e1)
    assertType t2 i32Type (hasPosition e2)
    return $ TypedExpression (I32DoubleOperatorExpression operator e1' e2') i32Type

-- instance TypeCheck A.IfExpression' where
--   typeCheck :: A.IfExpression' A.BNFC'Position -> PreprocessorMonad (A.IfExpression' Annotation)
--   typeCheck x = do
--     throwError $ Other "Not yet implemented" (hasPosition x)

-- instance TypeCheck A.MatchArm' where
--   typeCheck :: A.MatchArm' A.BNFC'Position -> PreprocessorMonad (A.MatchArm' Annotation)
--   typeCheck x = do
--     throwError $ Other "Not yet implemented" (hasPosition x)

instance TypeCheck A.Literal TypedExpression where
  typeCheck :: A.Literal -> PreprocessorMonad TypedExpression
  typeCheck (A.LiteralChar p char) = do
    return $ TypedExpression (LiteralExpression $ VChar char) charType
  typeCheck (A.LiteralString p string) = do
      return $ TypedExpression (LiteralExpression $ VArray (length string) (fmap VChar string)) stringType
  typeCheck (A.LiteralInteger p integer) = do
      return $ TypedExpression (LiteralExpression $ VI32 (fromIntegral integer)) i32Type
  typeCheck (A.LiteralBoolean p (A.BoolTrue _)) = do
      return $ TypedExpression (LiteralExpression $ VBool True) boolType
  typeCheck (A.LiteralBoolean p (A.BoolFalse _)) = do
      return $ TypedExpression (LiteralExpression $ VBool False) boolType

-- instance TypeCheck A.StructExpressionField' where
--   typeCheck :: A.StructExpressionField' A.BNFC'Position -> PreprocessorMonad (A.StructExpressionField' Annotation)
--   typeCheck x = do
--     throwError $ Other "Not yet implemented" (hasPosition x)

-- instance TypeCheck A.ArrayElement' where
--   typeCheck :: A.ArrayElement' A.BNFC'Position -> PreprocessorMonad (A.ArrayElement' Annotation)
--   typeCheck x = do
--     throwError $ Other "Not yet implemented" (hasPosition x)

-- instance TypeCheck A.Capture' where
--   typeCheck :: A.Capture' A.BNFC'Position -> PreprocessorMonad (A.Capture' Annotation)
--   typeCheck (A.Capture p typeModifier name) = do
--     throwError $ Other "Not yet implemented" p

-- instance TypeCheck A.CallParam' where
--   typeCheck :: A.CallParam' A.BNFC'Position -> PreprocessorMonad (A.CallParam' Annotation)
--   typeCheck (A.CallParam p expression) = do
--     expression' <- typeCheck expression
--     return $ A.CallParam (repositionAnnotation expression' p) expression'

-- instance TypeCheck A.UnaryOperator'

-- instance TypeCheck A.AssignmentOperator'

-- instance TypeCheck A.ComparisonOperator'

-- instance TypeCheck A.Pattern' where
--   typeCheck :: A.Pattern' A.BNFC'Position -> PreprocessorMonad (A.Pattern' Annotation)
--   typeCheck x = do
--     throwError $ Other "Not yet implemented" (hasPosition x)

-- instance TypeCheck A.Boolean'
