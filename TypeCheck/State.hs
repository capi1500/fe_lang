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

-- data PlaceContextType = ImmutablePlaceContext | MutablePlaceContext | AssigneeContet
data ExpressionContext = PlaceContext Mutable | ValueContext
    deriving (Eq, Ord, Show, Read)
data ExpressionType = PlaceType Mutable VariableId | ValueType Value
    deriving (Eq, Ord, Show, Read)

data Expectations = Expectations {
    currentFunctionReturnType :: Type,
    callParamType :: Maybe Type,
    insideLoopExpression :: Bool
} deriving (Eq, Ord, Show, Read)

data PreprocessorState = PreprocessorState {
    typeDefinitions :: TypeDefinitions, -- follows code scopes
    variables :: Variables, -- follows code scopes, resets on function frames
    lifetimeState :: LifetimeState, -- current lifetime
    warnings :: [PreprocessorWarning], -- only grows
    context :: ExpressionContext,
    toDropAtStatementEnd :: [VariableId],
    position :: BNFC'Position,
    expectations :: Expectations
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
    context = ValueContext,
    toDropAtStatementEnd = [],
    position = Nothing,
    expectations = Expectations {
        currentFunctionReturnType = unitType,
        callParamType = Nothing,
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
    PreprocessorState _ variables currentLifetime warnings context toDropAtStatementEnd position expectations <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position expectations

putVariables :: Variables -> PreprocessorMonad ()
putVariables variables = do
    PreprocessorState typeDefinitions _ currentLifetime warnings context toDropAtStatementEnd position expectations <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position expectations

putLifetimeState :: LifetimeState -> PreprocessorMonad ()
putLifetimeState lifetimeState = do
    PreprocessorState typeDefinitions variables _ warnings context toDropAtStatementEnd position expectations <- get
    put $ PreprocessorState typeDefinitions variables lifetimeState warnings context toDropAtStatementEnd position expectations

putWarnings :: [PreprocessorWarning] -> PreprocessorMonad ()
putWarnings warnings = do
    PreprocessorState typeDefinitions variables currentLifetime _ context toDropAtStatementEnd position expectations <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position expectations

addWarning :: PreprocessorWarning -> PreprocessorMonad ()
addWarning warning = do
    PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position expectations <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime (warning:warnings) context toDropAtStatementEnd position expectations

putContext :: ExpressionContext -> PreprocessorMonad ()
putContext context = do
    PreprocessorState typeDefinitions variables currentLifetime warnings _ toDropAtStatementEnd position expectations <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position expectations

markVariableAsToDrop :: VariableId -> PreprocessorMonad ()
markVariableAsToDrop id = do
    PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position expectations <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings context (listPushBack id toDropAtStatementEnd) position expectations

clearVariablesToDrop :: PreprocessorMonad ()
clearVariablesToDrop = do
    PreprocessorState typeDefinitions variables currentLifetime warnings context _ position expectations <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings context [] position expectations

putPosition :: BNFC'Position -> PreprocessorMonad ()
putPosition position = do
    PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd _ expectations <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position expectations

putExpectations :: Expectations -> PreprocessorMonad ()
putExpectations expectations = do
    PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position _ <- get
    put $ PreprocessorState typeDefinitions variables currentLifetime warnings context toDropAtStatementEnd position expectations

throw :: PreprocessorError -> PreprocessorMonad a
throw = throwError

isPlaceContext :: ExpressionContext -> Bool
isPlaceContext (PlaceContext _) = True
isPlaceContext ValueContext = False

isValueContext :: ExpressionContext -> Bool
isValueContext (PlaceContext _) = False
isValueContext ValueContext = True
