module TypeCheck.State where

import Data.Map
import Control.Monad.State
import Control.Monad.Except

import Fe.Abs (BNFC'Position, Ident (Ident))

import Common.Utils
import Common.Scope
import Common.Types
import Common.Printer
import Common.AstPrinter

import TypeCheck.Error
import TypeCheck.Variable
import Data.List (intercalate)
import Common.Ast (Expression)
import Common.InternalFunctions hiding (makeInternalFunction)


type TypeDefinitions = Scope (Map Identifier Type)
type VariableMappings = Scope (Map Identifier VariableId)
data Variables = Variables VariableMappings [Variable]
    deriving (Eq, Ord, Show, Read)
data LifetimeState = LifetimeState Lifetime Int
    deriving (Eq, Ord, Show, Read)

data ExpressionContext = PlaceContext Mutable | ValueContext (Maybe Type)
    deriving (Eq, Ord, Show, Read)
data ExpressionType = PlaceType Mutable VariableId | ValueType Value
    deriving (Eq, Ord, Show, Read)

data Context = Context {
    currentFunctionReturnType :: Type,
    insideLoopExpression :: Bool
} deriving (Eq, Ord, Show, Read)

data PreprocessorState = PreprocessorState {
    typeDefinitions :: TypeDefinitions, -- follows code scopes
    variables :: Variables, -- follows code scopes, resets on function frames
    lifetimeState :: LifetimeState, -- current lifetime
    warnings :: [PreprocessorWarning], -- only grows
    expressionContext :: ExpressionContext,
    toDropAtStatementEnd :: [VariableId],
    position :: BNFC'Position,
    context :: Context
} deriving (Eq, Ord, Show, Read)

data TypedExpression = TypedExpression Expression ExpressionType -- expression, type of expression

type PreprocessorMonad a = ExceptT PreprocessorError (State PreprocessorState) a

makePreprocessorState :: PreprocessorState
makePreprocessorState = PreprocessorState {
    typeDefinitions = Global (
        fromList [
            ("i32", TPrimitive I32),
            ("char", TPrimitive Char),
            ("bool", TPrimitive Bool),
            ("()", TPrimitive Unit),
            ("String", TArray charType)]
    ),
    variables = fromInternals internals,
    lifetimeState = LifetimeState staticLifetime 1,
    warnings = [],
    expressionContext = ValueContext Nothing,
    toDropAtStatementEnd = [],
    position = Nothing,
    context = Context {
        currentFunctionReturnType = unitType,
        insideLoopExpression = False
    }
}

makeInternalFunction :: VariableId -> Identifier -> Type -> Variable
makeInternalFunction id name t = Variable {
    variableCreatedAt = Nothing,
    variableName = Just name,
    variableType = t,
    variableId = id,
    variableMutability = Const,
    variableState = Free,
    variableValue = Value {
        valueCreatedAt = Nothing,
        valueType = t,
        ownedPlaces = [],
        borrows = [],
        borrowsMut = [],
        owned = True
    },
    lifetime = staticLifetime
}

fromInternals :: [(Identifier, Type, a)] -> Variables
fromInternals names =
    let zipped = zip names [0..(length names)] in
    Variables
        (Global $ fromList (fmap (\((name, t, _), id) -> (name, id)) zipped))
        (fmap (\((name, t, _), id) -> makeInternalFunction id name t) zipped)

staticLifetime = Lifetime [0] 1

putTypeDefinitions :: TypeDefinitions -> PreprocessorMonad ()
putTypeDefinitions typeDefinitions = do
    PreprocessorState _ variables currentLifetime warnings expressionContext toDropAtStatementEnd position context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position context

putVariables :: Variables -> PreprocessorMonad ()
putVariables variables = do
    PreprocessorState typeDefinitions _ currentLifetime warnings expressionContext toDropAtStatementEnd position context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position context

putLifetimeState :: LifetimeState -> PreprocessorMonad ()
putLifetimeState lifetimeState = do
    PreprocessorState typeDefinitions variables _ warnings expressionContext toDropAtStatementEnd position context <- get
    put $ PreprocessorState typeDefinitions variables lifetimeState warnings expressionContext toDropAtStatementEnd position context

putWarnings :: [PreprocessorWarning] -> PreprocessorMonad ()
putWarnings warnings = do
    PreprocessorState typeDefinitions variables currentLifetime _ expressionContext toDropAtStatementEnd position context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position context

addWarning :: PreprocessorWarning -> PreprocessorMonad ()
addWarning warning = do
    PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime (warning:warnings) expressionContext toDropAtStatementEnd position context

putExpressionContext :: ExpressionContext -> PreprocessorMonad ()
putExpressionContext expressionContext = do
    PreprocessorState typeDefinitions variables currentLifetime warnings _ toDropAtStatementEnd position context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position context

markVariableAsToDrop :: VariableId -> PreprocessorMonad ()
markVariableAsToDrop id = do
    PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext (listPushBack id toDropAtStatementEnd) position context

clearVariablesToDrop :: PreprocessorMonad ()
clearVariablesToDrop = do
    PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext _ position context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext [] position context

putPosition :: BNFC'Position -> PreprocessorMonad ()
putPosition position = do
    PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd _ context <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position context

putContext :: Context -> PreprocessorMonad ()
putContext context = do
    PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position _ <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings expressionContext toDropAtStatementEnd position context

throw :: PreprocessorError -> PreprocessorMonad a
throw = throwError

isPlaceContext :: ExpressionContext -> Bool
isPlaceContext (PlaceContext _) = True
isPlaceContext (ValueContext _) = False

isValueContext :: ExpressionContext -> Bool
isValueContext (PlaceContext _) = False
isValueContext (ValueContext _) = True
