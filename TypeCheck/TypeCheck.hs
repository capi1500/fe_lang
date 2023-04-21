{-# LANGUAGE InstanceSigs #-}
module TypeCheck.TypeCheck where

import qualified Fe.Abs as A
import Common.Ast
import Control.Monad.Except
import Control.Monad.State
import Prelude (Integer, String, IO, ($), Traversable (traverse), Maybe (Just), (++), Show (show), Eq (..), last, Foldable (null))
import qualified Prelude as C (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Int, Maybe(..))
import Fe.Abs (HasPosition(hasPosition))
import TypeCheck.Utils
import Common.Utils (getItemIdent)
import Common.Types
import Data.Maybe (isNothing)
import Common.Scope (Scope(..))
import Data.Map (empty)

class C.Functor a => TypeCheck a where
    typeCheck :: a A.BNFC'Position -> TypeCheckM (a Annotation)
    typeCheck x = do
        return $ fmap Position x

instance TypeCheck A.Code' where
    typeCheck :: A.Code' A.BNFC'Position -> TypeCheckM (A.Code' Annotation)
    typeCheck (A.Code p statements) = do
        statements' <- traverse typeCheck statements
        return $ A.Code (Position p) statements'

instance TypeCheck A.Statement' where
    typeCheck :: A.Statement' A.BNFC'Position -> TypeCheckM (A.Statement' Annotation)
    typeCheck (A.SemicolonStatement p) = do
        return $ A.SemicolonStatement (Position p)
    typeCheck (A.ItemStatement p item) = do
        t <- preTypeCheck item
        tryAddIdentToScope (getItemIdent item) t p
        item' <- typeCheck item
        return $ A.ItemStatement (Position p) item'
    typeCheck (A.ExpressionStatement p expression) = do
        expression' <- typeCheck expression
        return $ A.ExpressionStatement (Position p) expression'

instance TypeCheck A.Item' where
  typeCheck :: A.Item' A.BNFC'Position -> TypeCheckM (A.Item' Annotation)
  typeCheck (A.ItemFunction p function) = do
    function' <- typeCheck function
    return $ A.ItemFunction (repositionAnnotation function' p) function'
  typeCheck (A.ItemStruct p strut) = do
    strut' <- typeCheck strut
    return $ A.ItemStruct (repositionAnnotation strut' p) strut'
  typeCheck (A.ItemVariant p variant) = do
    variant' <- typeCheck variant
    return $ A.ItemVariant (repositionAnnotation variant' p) variant'
  typeCheck (A.ItemVariable p variable) = do
    variable' <- typeCheck variable
    return $ A.ItemVariable (repositionAnnotation variable' p) variable'
  typeCheck (A.ItemConst p const) = do
    const' <- typeCheck const
    return $ A.ItemConst (repositionAnnotation const' p) const'

preTypeCheck :: A.Item' A.BNFC'Position -> TypeCheckM Type
preTypeCheck (A.ItemFunction _ function) = preTypeCheckFunction function
preTypeCheck (A.ItemStruct _ struct) = preTypeCheckStruct struct
preTypeCheck (A.ItemVariant _ variant) = preTypeCheckVariant variant
preTypeCheck (A.ItemVariable _ variable) = preTypeCheckVariable variable
preTypeCheck (A.ItemConst _ const) = preTypeCheckConst const

preTypeCheckFunction :: A.Function' A.BNFC'Position -> TypeCheckM Type
preTypeCheckFunction (A.Function p ident params returnType _) = do
    params' <- traverse typeCheck params
    returnType' <- typeCheck returnType
    return $ TFunction $ Function p (fmap getAnnotatedType params') (getAnnotatedType returnType')

preTypeCheckStruct :: A.Struct' A.BNFC'Position -> TypeCheckM Type
preTypeCheckStruct x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

preTypeCheckVariant :: A.Variant' A.BNFC'Position -> TypeCheckM Type
preTypeCheckVariant x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

preTypeCheckVariable :: A.Variable' A.BNFC'Position -> TypeCheckM Type
preTypeCheckVariable x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

preTypeCheckConst :: A.Const' A.BNFC'Position -> TypeCheckM Type
preTypeCheckConst x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.Function' where
  typeCheck :: A.Function' A.BNFC'Position -> TypeCheckM (A.Function' Annotation)
  typeCheck (A.Function p ident params returnType code) = do
    params' <- traverse typeCheck params
    code' <- typeCheck code
    returnType' <- typeCheck returnType
    let t = TFunction $ Function p (fmap getAnnotatedType params') (getAnnotatedType returnType')
    return $ A.Function (Typed p t) ident params' returnType' code'

instance TypeCheck A.FunctionParam' where
  typeCheck :: A.FunctionParam' A.BNFC'Position -> TypeCheckM (A.FunctionParam' Annotation)
  typeCheck (A.Parameter p ident t) = do
    t' <- typeCheck t
    return $ A.Parameter (repositionAnnotation t' p) ident t'

instance TypeCheck A.FunctionReturnType' where
  typeCheck :: A.FunctionReturnType' A.BNFC'Position -> TypeCheckM (A.FunctionReturnType' Annotation)
  typeCheck (A.ReturnValue p t) = do
    t' <- typeCheck t
    return $ A.ReturnValue (repositionAnnotation t' p) t'
  typeCheck (A.ReturnUnit p) = do
    return $ A.ReturnUnit (Typed p (TPrimitive Unit))

instance TypeCheck A.Struct' where
  typeCheck :: A.Struct' A.BNFC'Position -> TypeCheckM (A.Struct' Annotation)
  typeCheck (A.Struct p ident fields) = do
    fields' <- traverse typeCheck fields
    let mapper (A.StructField _ ident t) = Field ident (getAnnotatedType t)
    let t = TStruct $ Struct p (fmap mapper fields')
    return $ A.Struct (Typed p t) ident fields'

instance TypeCheck A.StructField' where
  typeCheck :: A.StructField' A.BNFC'Position -> TypeCheckM (A.StructField' Annotation)
  typeCheck (A.StructField p ident t) = do
    t' <- typeCheck t -- allowing all, even unsized arrays and self recursion
    return $ A.StructField (Typed p (getAnnotatedType t')) ident t'

instance TypeCheck A.Variant' where
  typeCheck :: A.Variant' A.BNFC'Position -> TypeCheckM (A.Variant' Annotation)
  typeCheck (A.Variant p ident items) = do
    items' <- traverse typeCheck items
    let t = TVariant $ Variant p (fmap getAnnotatedType items')
    return $ A.Variant (Typed p t) ident items'

instance TypeCheck A.VariantItem' where
  typeCheck :: A.VariantItem' A.BNFC'Position -> TypeCheckM (A.VariantItem' Annotation)
  typeCheck (A.VariantItem p t) = do
    t' <- typeCheck t -- allowing all, even unsized arrays and self recursion
    return $ A.VariantItem (Typed p (getAnnotatedType t')) t'

instance TypeCheck A.Variable' where
  typeCheck :: A.Variable' A.BNFC'Position -> TypeCheckM (A.Variable' Annotation)
  typeCheck (A.Variable p cv) = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.Const' where
  typeCheck :: A.Const' A.BNFC'Position -> TypeCheckM (A.Const' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.CVDeclaration' where
  typeCheck :: A.CVDeclaration' A.BNFC'Position -> TypeCheckM (A.CVDeclaration' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.TypeDeclaration' where
  typeCheck :: A.TypeDeclaration' A.BNFC'Position -> TypeCheckM (A.TypeDeclaration' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.Type' where
  typeCheck :: A.Type' A.BNFC'Position -> TypeCheckM (A.Type' Annotation)
  typeCheck (A.TypeSimpleType p modifier simpleType) = do
    modifier' <- typeCheck modifier
    simpleType' <- typeCheck simpleType
    let A.SimpleType position ident = simpleType
    t <- getTypeOfIdent ident position
    return $ A.TypeSimpleType (Typed p (modifyType modifier t)) modifier' simpleType'
  typeCheck (A.TypeArrayType p modifier arrayType) = do
    throwError $ TypeCheckError "Not yet implemented" p
  typeCheck (A.TypeFunctionType p functionType) = do
    throwError $ TypeCheckError "Not yet implemented" p

instance TypeCheck A.TypeModifier' where
  typeCheck :: A.TypeModifier' A.BNFC'Position -> TypeCheckM (A.TypeModifier' Annotation)
  typeCheck (A.None p) = do
    return $ A.None (Position p)
  typeCheck (A.Ref p) = do
    throwError $ TypeCheckError "Not yet implemented" p
  typeCheck (A.RefLifetime p lifetime) = do
    throwError $ TypeCheckError "Not yet implemented" p
  typeCheck (A.MutRef p) = do
    throwError $ TypeCheckError "Not yet implemented" p
  typeCheck (A.MutRefLifetime p lifetime) = do
    throwError $ TypeCheckError "Not yet implemented" p

instance TypeCheck A.Lifetime' where
  typeCheck :: A.Lifetime' A.BNFC'Position -> TypeCheckM (A.Lifetime' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.SimpleType' where
  typeCheck :: A.SimpleType' A.BNFC'Position -> TypeCheckM (A.SimpleType' Annotation)
  typeCheck (A.SimpleType p ident) = do
    t <- getTypeOfIdent ident p
    return $ A.SimpleType (Typed p t) ident

instance TypeCheck A.ArrayType' where
  typeCheck :: A.ArrayType' A.BNFC'Position -> TypeCheckM (A.ArrayType' Annotation)
  typeCheck (A.ArrayTypeSized p t expression) = do
    t' <- typeCheck t
    expression' <- typeCheck expression
    when (getAnnotatedType expression' /= TPrimitive I32) $ do throwError (TypeCheckError "a" p)
    return $ A.ArrayTypeSized (Typed p (TArray $ Array (getAnnotatedType t') UnSized)) t' expression'
  typeCheck (A.ArrayType p t) = do
    t' <- typeCheck t
    return $ A.ArrayType (Typed p (TArray $ Array (getAnnotatedType t') UnSized)) t'

instance TypeCheck A.FunctionType' where
  typeCheck :: A.FunctionType' A.BNFC'Position -> TypeCheckM (A.FunctionType' Annotation)
  typeCheck (A.FunctionType p parameters returnType) = do
    parameters' <- traverse typeCheck parameters
    returnType' <- typeCheck returnType
    let t = TFunction $ Function p (fmap getAnnotatedType parameters') (getAnnotatedType returnType')
    return $ A.FunctionType (Typed p t) parameters' returnType'

instance TypeCheck A.FunctionTypeParam' where
  typeCheck :: A.FunctionTypeParam' A.BNFC'Position -> TypeCheckM (A.FunctionTypeParam' Annotation)
  typeCheck (A.FunctionTypeParam p t) = do
    t' <- typeCheck t
    return $ A.FunctionTypeParam (Typed p (getAnnotatedType t')) t'

instance TypeCheck A.FunctionTypeReturnType' where
  typeCheck :: A.FunctionTypeReturnType' A.BNFC'Position -> TypeCheckM (A.FunctionTypeReturnType' Annotation)
  typeCheck (A.FunctionTypeReturnType p t) = do
    t' <- typeCheck t
    return $ A.FunctionTypeReturnType (Typed p (TPrimitive Unit)) t'
  typeCheck (A.FunctionTypeReturnTypeUnit p) = do
    return $ A.FunctionTypeReturnTypeUnit (Typed p (TPrimitive Unit))

instance TypeCheck A.Expression' where
  typeCheck :: A.Expression' A.BNFC'Position -> TypeCheckM (A.Expression' Annotation)
  typeCheck (A.BlockExpression p statements) = do
    parentScope <- get
    put $ Local parentScope empty
    statements' <- traverse typeCheck statements
    put parentScope
    let annotation = if null statements' then Position p
        else repositionAnnotation (last statements') p
    return $ A.BlockExpression annotation statements'
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.IfExpression' where
  typeCheck :: A.IfExpression' A.BNFC'Position -> TypeCheckM (A.IfExpression' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.MatchArm' where
  typeCheck :: A.MatchArm' A.BNFC'Position -> TypeCheckM (A.MatchArm' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.Literal' where
  typeCheck :: A.Literal' A.BNFC'Position -> TypeCheckM (A.Literal' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.StructExpressionField' where
  typeCheck :: A.StructExpressionField' A.BNFC'Position -> TypeCheckM (A.StructExpressionField' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.ArrayElement' where
  typeCheck :: A.ArrayElement' A.BNFC'Position -> TypeCheckM (A.ArrayElement' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.Capture' where
  typeCheck :: A.Capture' A.BNFC'Position -> TypeCheckM (A.Capture' Annotation)
  typeCheck (A.Capture p typeModifier name) = do
    throwError $ TypeCheckError "Not yet implemented" p
    -- typeModifier' <- typeCheck typeModifier
    -- type' <- getTypeOfIdent name p
    -- return $ A.Capture (Typed p type') typeModifier' name

instance TypeCheck A.CallParam' where
  typeCheck :: A.CallParam' A.BNFC'Position -> TypeCheckM (A.CallParam' Annotation)
  typeCheck (A.CallParam p expression) = do
    expression' <- typeCheck expression
    return $ A.CallParam (repositionAnnotation expression' p) expression'

instance TypeCheck A.UnaryOperator'

instance TypeCheck A.AssignmentOperator'

instance TypeCheck A.ComparisonOperator'

instance TypeCheck A.Pattern' where
  typeCheck :: A.Pattern' A.BNFC'Position -> TypeCheckM (A.Pattern' Annotation)
  typeCheck x = do
    throwError $ TypeCheckError "Not yet implemented" (hasPosition x)

instance TypeCheck A.Boolean'
