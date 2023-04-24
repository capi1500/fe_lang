{-# LANGUAGE InstanceSigs #-}
module TypeCheck.TypeCheck where

import Data.Maybe (isNothing)
import Data.Map (empty)
import Data.Foldable (traverse_)
import Control.Monad.Except
import Control.Monad.State
import qualified Fe.Abs as A
import Fe.Abs (HasPosition(hasPosition))
import Common.Ast
import Common.Utils
import Common.Types
import TypeCheck.Utils
import TypeCheck.State
import TypeCheck.Error

class Functor a => TypeCheck a where
    typeCheck :: a A.BNFC'Position -> PreprocessorMonad (a Annotation)
    typeCheck x = do
        return $ fmap Position x

instance TypeCheck A.Code' where
    typeCheck :: A.Code' A.BNFC'Position -> PreprocessorMonad (A.Code' Annotation)
    typeCheck (A.Code p statements) = do
        traverse_ initializeGlobalScope statements
        statements' <- traverse typeCheck statements
        return $ A.Code (Position p) statements'

initializeGlobalScope :: A.Statement' A.BNFC'Position -> PreprocessorMonad ()
initializeGlobalScope (A.ItemStatement _ item) = addToScope item
initializeGlobalScope _ = do return ()

addToScope :: A.Item' A.BNFC'Position -> PreprocessorMonad ()
addToScope (A.ItemFunction _ (A.Function p ident params returnType _)) = do
    params' <- traverse typeCheck params
    returnType' <- typeCheck returnType
    let t = TFunction $ Function (NamedFunction (Identifier p ident)) Fn Static (fmap getAnnotatedType params') (getAnnotatedType returnType')
    return ()
addToScope (A.ItemStruct p struct) = do
    throwError $ Other "Not yet implemented" p
addToScope (A.ItemVariant p variant) = do
    throwError $ Other "Not yet implemented" p
addToScope (A.ItemVariable p (A.Const _) ident typeDeclaration initialization) = do
    throwError $ Other "Not yet implemented" p
addToScope (A.ItemVariable p (A.Var _) ident typeDeclaration initialization) = do
    PreprocessorState scope allocator <- get
    when (isGlobal scope) $ do throwError $ VariableAtGlobalScope (Identifier p ident)
    throwError $ Other "Not yet implemented" p

instance TypeCheck A.Statement' where
    typeCheck :: A.Statement' A.BNFC'Position -> PreprocessorMonad (A.Statement' Annotation)
    typeCheck (A.SemicolonStatement p) = do
        return $ A.SemicolonStatement (Position p)
    typeCheck (A.ItemStatement p item) = do
        item' <- typeCheck item
        return $ A.ItemStatement (Position p) item'
    typeCheck (A.ExpressionStatement p expression) = do
        expression' <- typeCheck expression
        return $ A.ExpressionStatement (Position p) expression'

instance TypeCheck A.Item' where
  typeCheck :: A.Item' A.BNFC'Position -> PreprocessorMonad (A.Item' Annotation)
  typeCheck (A.ItemFunction p function) = do
    function' <- typeCheck function
    return $ A.ItemFunction (repositionAnnotation function' p) function'
  typeCheck (A.ItemStruct p strut) = do
    strut' <- typeCheck strut
    return $ A.ItemStruct (repositionAnnotation strut' p) strut'
  typeCheck (A.ItemVariant p variant) = do
    variant' <- typeCheck variant
    return $ A.ItemVariant (repositionAnnotation variant' p) variant'
  typeCheck (A.ItemVariable p cv ident typeDeclaration initialization) = do
    cv' <- typeCheck cv
    typeDeclaration' <- typeCheck typeDeclaration
    initialization' <- typeCheck initialization
    let typeDeclarationType = getAnnotatedType typeDeclaration'
    let initializationType = getAnnotatedType initialization'
    let typesMatch =
            typeDeclarationType == TUntyped ||
            initializationType == TUntyped ||
            typeDeclarationType == initializationType
    unless typesMatch $ do
        throwError $ TypeMismatch (Identifier p ident) typeDeclarationType initializationType
    let t = Typed
            p
            (if typeDeclarationType == TUntyped then
                initializationType
            else
                typeDeclarationType)
    return $ A.ItemVariable t cv' ident typeDeclaration' initialization'

instance TypeCheck A.Function' where
  typeCheck :: A.Function' A.BNFC'Position -> PreprocessorMonad (A.Function' Annotation)
  typeCheck (A.Function p ident params returnType code) = do
    params' <- traverse typeCheck params
    code' <- typeCheck code
    returnType' <- typeCheck returnType
    let t = TFunction $ Function
            (NamedFunction (Identifier p ident))
            Fn
            Static
            (fmap getAnnotatedType params')
            (getAnnotatedType returnType')
    return $ A.Function (Typed p t) ident params' returnType' code'

instance TypeCheck A.FunctionParam' where
  typeCheck :: A.FunctionParam' A.BNFC'Position -> PreprocessorMonad (A.FunctionParam' Annotation)
  typeCheck (A.Parameter p ident t) = do
    t' <- typeCheck t
    return $ A.Parameter (repositionAnnotation t' p) ident t'

instance TypeCheck A.FunctionReturnType' where
  typeCheck :: A.FunctionReturnType' A.BNFC'Position -> PreprocessorMonad (A.FunctionReturnType' Annotation)
  typeCheck (A.ReturnValue p t) = do
    t' <- typeCheck t
    return $ A.ReturnValue (repositionAnnotation t' p) t'
  typeCheck (A.ReturnUnit p) = do
    return $ A.ReturnUnit (Typed p (TPrimitive Unit))

instance TypeCheck A.Struct' where
  typeCheck :: A.Struct' A.BNFC'Position -> PreprocessorMonad (A.Struct' Annotation)
  typeCheck (A.Struct p ident fields) = do
    fields' <- traverse typeCheck fields
    let identifier = Identifier p ident
    let mapper (A.StructField _ ident t) = Field identifier (getAnnotatedType t)
    let t = TStruct $ Struct identifier (fmap mapper fields')
    return $ A.Struct (Typed p t) ident fields'

instance TypeCheck A.StructField' where
  typeCheck :: A.StructField' A.BNFC'Position -> PreprocessorMonad (A.StructField' Annotation)
  typeCheck (A.StructField p ident t) = do
    t' <- typeCheck t -- allowing all, even unsized arrays and self recursion
    return $ A.StructField (Typed p (getAnnotatedType t')) ident t'

instance TypeCheck A.Variant' where
  typeCheck :: A.Variant' A.BNFC'Position -> PreprocessorMonad (A.Variant' Annotation)
  typeCheck (A.Variant p ident items) = do
    items' <- traverse typeCheck items
    let t = TVariant $ Variant (Identifier p ident) (fmap getAnnotatedType items')
    return $ A.Variant (Typed p t) ident items'

instance TypeCheck A.VariantSubtype' where
  typeCheck :: A.VariantSubtype' A.BNFC'Position -> PreprocessorMonad (A.VariantSubtype' Annotation)
  typeCheck (A.VariantSubtype p t) = do
    t' <- typeCheck t -- allowing all, even unsized arrays and self recursion
    return $ A.VariantSubtype (Typed p (getAnnotatedType t')) t'

instance TypeCheck A.CV'

instance TypeCheck A.Initialization'

instance TypeCheck A.TypeDeclaration' where
  typeCheck :: A.TypeDeclaration' A.BNFC'Position -> PreprocessorMonad (A.TypeDeclaration' Annotation)
  typeCheck (A.Untyped p) = do
    return $ A.Untyped (Typed p TUntyped)
  typeCheck (A.Typed p t) = do
    t' <- typeCheck t
    return $ A.Typed (Typed p (getAnnotatedType t')) t'

instance TypeCheck A.Type' where
  typeCheck :: A.Type' A.BNFC'Position -> PreprocessorMonad (A.Type' Annotation)
  typeCheck (A.TypeSimpleType p modifier simpleType) = do
    modifier' <- typeCheck modifier
    simpleType' <- typeCheck simpleType
    let A.SimpleType position ident = simpleType
    t <- getType (Identifier position ident)
    return $ A.TypeSimpleType (Typed p (modifyType modifier t)) modifier' simpleType'
  typeCheck (A.TypeArrayType p modifier arrayType) = do
    throwError $ Other "Not yet implemented" p
  typeCheck (A.TypeFunctionType p functionType) = do
    throwError $ Other "Not yet implemented" p

instance TypeCheck A.TypeModifier' where
  typeCheck :: A.TypeModifier' A.BNFC'Position -> PreprocessorMonad (A.TypeModifier' Annotation)
  typeCheck (A.None p) = do
    return $ A.None (Position p)
  typeCheck (A.Ref p lifetime) = do
    throwError $ Other "Not yet implemented" p
  typeCheck (A.MutRef p lifetime) = do
    throwError $ Other "Not yet implemented" p

instance TypeCheck A.Lifetime' where
  typeCheck :: A.Lifetime' A.BNFC'Position -> PreprocessorMonad (A.Lifetime' Annotation)
  typeCheck (A.ExplicitLifetime p ident) = do
    throwError $ Other "Not yet implemented" p
  typeCheck x = do
        return $ fmap Position x -- TODO: deal with lifetimes, assume static for now

instance TypeCheck A.SimpleType' where
  typeCheck :: A.SimpleType' A.BNFC'Position -> PreprocessorMonad (A.SimpleType' Annotation)
  typeCheck (A.SimpleType p ident) = do
    t <- getType (Identifier p ident)
    return $ A.SimpleType (Typed p t) ident

instance TypeCheck A.ArrayType' where
  typeCheck :: A.ArrayType' A.BNFC'Position -> PreprocessorMonad (A.ArrayType' Annotation)
  typeCheck (A.ArrayTypeSized p t expression) = do
    t' <- typeCheck t
    expression' <- typeCheck expression
    when (getAnnotatedType expression' /= TPrimitive I32) $ do throwError (Other "a" p)
    return $ A.ArrayTypeSized (Typed p (TArray $ Array (getAnnotatedType t') UnSized)) t' expression'
  typeCheck (A.ArrayType p t) = do
    t' <- typeCheck t
    return $ A.ArrayType (Typed p (TArray $ Array (getAnnotatedType t') UnSized)) t'

instance TypeCheck A.FunctionType' where
  typeCheck :: A.FunctionType' A.BNFC'Position -> PreprocessorMonad (A.FunctionType' Annotation)
  typeCheck (A.FunctionType p functionKind lifetime parameters returnType) = do
    functionKind' <- typeCheck functionKind
    lifetime' <- typeCheck lifetime
    parameters' <- traverse typeCheck parameters
    returnType' <- typeCheck returnType
    let t = TFunction $ Function
            Unnamed
            (case functionKind' of
            A.Once _ -> FnOnce
            A.Normal _ -> Fn)
            Static
            (fmap getAnnotatedType parameters') (getAnnotatedType returnType')
    return $ A.FunctionType (Typed p t) functionKind' lifetime' parameters' returnType'

instance TypeCheck A.FunctionKind'

instance TypeCheck A.FunctionTypeParam' where
  typeCheck :: A.FunctionTypeParam' A.BNFC'Position -> PreprocessorMonad (A.FunctionTypeParam' Annotation)
  typeCheck (A.FunctionTypeParam p t) = do
    t' <- typeCheck t
    return $ A.FunctionTypeParam (Typed p (getAnnotatedType t')) t'

instance TypeCheck A.FunctionTypeReturnType' where
  typeCheck :: A.FunctionTypeReturnType' A.BNFC'Position -> PreprocessorMonad (A.FunctionTypeReturnType' Annotation)
  typeCheck (A.FunctionTypeReturnType p t) = do
    t' <- typeCheck t
    return $ A.FunctionTypeReturnType (Typed p (TPrimitive Unit)) t'
  typeCheck (A.FunctionTypeReturnTypeUnit p) = do
    return $ A.FunctionTypeReturnTypeUnit (Typed p (TPrimitive Unit))

instance TypeCheck A.Expression' where
  typeCheck :: A.Expression' A.BNFC'Position -> PreprocessorMonad (A.Expression' Annotation)
  typeCheck (A.BlockExpression p statements) = do
    throwError $ Other "Not yet implemented" p
    -- TODO: fill this now
  typeCheck (A.LiteralExpression p literal) = do
    literal' <- typeCheck literal
    return $ A.LiteralExpression (Typed p (getAnnotatedType literal')) literal'
  typeCheck x = do
    throwError $ Other "Not yet implemented" (hasPosition x)

instance TypeCheck A.IfExpression' where
  typeCheck :: A.IfExpression' A.BNFC'Position -> PreprocessorMonad (A.IfExpression' Annotation)
  typeCheck x = do
    throwError $ Other "Not yet implemented" (hasPosition x)

instance TypeCheck A.MatchArm' where
  typeCheck :: A.MatchArm' A.BNFC'Position -> PreprocessorMonad (A.MatchArm' Annotation)
  typeCheck x = do
    throwError $ Other "Not yet implemented" (hasPosition x)

instance TypeCheck A.Literal' where
  typeCheck :: A.Literal' A.BNFC'Position -> PreprocessorMonad (A.Literal' Annotation)
  typeCheck (A.LiteralChar p char) = do
    return $ A.LiteralChar (Typed p (TPrimitive Char)) char
  typeCheck (A.LiteralString p string) = do
    return $ A.LiteralString (Typed p stringType) string
  typeCheck (A.LiteralInteger p integer) = do
    return $ A.LiteralInteger (Typed p (TPrimitive I32)) integer
  typeCheck (A.LiteralBoolean p boolean) = do
    return $ A.LiteralBoolean (Typed p (TPrimitive Bool)) boolean

instance TypeCheck A.StructExpressionField' where
  typeCheck :: A.StructExpressionField' A.BNFC'Position -> PreprocessorMonad (A.StructExpressionField' Annotation)
  typeCheck x = do
    throwError $ Other "Not yet implemented" (hasPosition x)

instance TypeCheck A.ArrayElement' where
  typeCheck :: A.ArrayElement' A.BNFC'Position -> PreprocessorMonad (A.ArrayElement' Annotation)
  typeCheck x = do
    throwError $ Other "Not yet implemented" (hasPosition x)

instance TypeCheck A.Capture' where
  typeCheck :: A.Capture' A.BNFC'Position -> PreprocessorMonad (A.Capture' Annotation)
  typeCheck (A.Capture p typeModifier name) = do
    throwError $ Other "Not yet implemented" p

instance TypeCheck A.CallParam' where
  typeCheck :: A.CallParam' A.BNFC'Position -> PreprocessorMonad (A.CallParam' Annotation)
  typeCheck (A.CallParam p expression) = do
    expression' <- typeCheck expression
    return $ A.CallParam (repositionAnnotation expression' p) expression'

instance TypeCheck A.UnaryOperator'

instance TypeCheck A.AssignmentOperator'

instance TypeCheck A.ComparisonOperator'

instance TypeCheck A.Pattern' where
  typeCheck :: A.Pattern' A.BNFC'Position -> PreprocessorMonad (A.Pattern' Annotation)
  typeCheck x = do
    throwError $ Other "Not yet implemented" (hasPosition x)

instance TypeCheck A.Boolean'
