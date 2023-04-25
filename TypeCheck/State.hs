module TypeCheck.State where
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (isNothing, Maybe (..), fromJust, isJust)
import Data.Map (Map, empty, fromList, insert, lookup)
import Fe.Abs (Ident (..), BNFC'Position)
import Common.Utils
import Common.Types
import TypeCheck.Error
import Prelude hiding (lookup)
import Common.Scope
import Distribution.Fields.LexerMonad (LexState(warnings))

data VariableState =
    Uninitialized |
    Borrowed Integer BNFC'Position |
    BorrowedMut BNFC'Position |
    Free
  deriving (Eq, Ord, Show, Read)

data Variable = Variable Identifier Type VariableState
  deriving (Eq, Ord, Show, Read)

type VariableMappings = Map Ident VariableId
type TypeDefinitions = Map Ident Type
type PreprocessorScope = Scope (VariableMappings, TypeDefinitions)

data Allocator = Allocator [Variable] [VariableId] VariableId
  deriving (Eq, Ord, Show, Read)

data PreprocessorState = PreprocessorState PreprocessorScope Allocator [PreprocessorWarning] VariableId
  deriving (Eq, Ord, Show, Read)

type PreprocessorMonad a = StateT PreprocessorState (Except PreprocessorError) a

makePreprocessorState :: PreprocessorState
makePreprocessorState = PreprocessorState
    (Global (empty,
        fromList [
            (Ident "i32", TPrimitive I32),
            (Ident "char", TPrimitive Char),
            (Ident "bool", TPrimitive Bool),
            (Ident "()", TPrimitive Unit),
            (Ident "String", TArray $ Array (TPrimitive Char) UnSized)
    ]))
    (Allocator [] [] 0)
    []
    (-1) -- mainFunction id

getType :: Identifier -> PreprocessorMonad Type
getType identifier = do
    let (Identifier p ident) = identifier
    PreprocessorState scope _ _ _ <- get
    let maybeType = helper ident scope
    when (isNothing maybeType) $ do throwError $ TypeNotDefined identifier
    return $ fromJust maybeType
  where
    helper ident (Global (_, types)) = lookup ident types
    helper ident (Local parent (_, types)) =
        let x = lookup ident types in
        if isNothing x then helper ident parent
        else x

getTypeLocal :: Identifier -> PreprocessorScope -> Maybe Type
getTypeLocal (Identifier _ ident) (Global (_, types)) = lookup ident types
getTypeLocal (Identifier _ ident) (Local _ (_, types)) = lookup ident types

addType :: Identifier -> Type -> PreprocessorMonad ()
addType identifier t = do
    let (Identifier _ ident) = identifier
    PreprocessorState scope allocator warnings mainId <- get
    let maybeDefinition = getTypeLocal identifier scope
    when (isJust maybeDefinition) $ do throwError $ TypeAlreadyInScope identifier t (fromJust maybeDefinition)
    put $ PreprocessorState
        (helper ident t scope)
        allocator
        warnings
        mainId
  where
    helper ident t (Global (variables, types)) = Global (variables, insert ident t types)
    helper ident t (Local parent (variables, types)) = Local parent (variables, insert ident t types)

getVariable :: Identifier -> PreprocessorMonad (VariableId, Variable)
getVariable identifier = do
    let (Identifier _ ident) = identifier
    PreprocessorState scope (Allocator variableStack _ _) _ _ <- get
    let maybeId = helper ident scope
    when (isNothing maybeId) $ do throwError $ VariableNotDefined identifier
    let id = fromJust maybeId
    return (id, listGet id variableStack)
  where
    helper ident (Global (variables, _)) = lookup ident variables
    helper ident (Local parent (variables, _)) =
        let x = lookup ident variables in
        if isNothing x then helper ident parent
        else x

tryGetVariableLocal :: Identifier -> PreprocessorMonad (Maybe (VariableId, Variable))
tryGetVariableLocal identifier = do
    PreprocessorState scope (Allocator variableStack _ _) _ _ <- get
    return $ do
        id <- helper identifier scope
        return (id, listGet id variableStack)
  where
    helper (Identifier _ ident) (Global (variables, _)) = lookup ident variables
    helper (Identifier _ ident) (Local _ (variables, _)) = lookup ident variables

addVariable :: Identifier -> Variable -> PreprocessorMonad VariableId
addVariable identifier variable = do
    let (Identifier _ ident) = identifier
    PreprocessorState scope allocator warnings mainId <- get
    let (id, allocator') = allocate allocator variable
    put $ PreprocessorState
        (helper ident id scope)
        allocator'
        warnings
        mainId
    return id
  where
    helper ident id (Global (variables, types)) = Global (insert ident id variables, types)
    helper ident id (Local parent (variables, types)) = Local parent (insert ident id variables, types)

allocate :: Allocator -> Variable -> (VariableId, Allocator)
allocate (Allocator variableStack freeCells maxStackSize) variable =
    let l = length variableStack in
    case freeCells of
        [] ->
            (l, Allocator (listPushBack variable variableStack) [] (max maxStackSize (l + 1)))
        (h:t) ->
            (h, Allocator (listSet h variable variableStack) t maxStackSize)

free :: VariableId -> PreprocessorMonad ()
free id = do
    PreprocessorState scope (Allocator variableStack freeCells maxStackSize) warnings mainId <- get
    let allocator =
            if length variableStack == id then
                Allocator (take (id - 1) variableStack) freeCells maxStackSize
            else
                Allocator variableStack (id:freeCells) maxStackSize
    put $ PreprocessorState scope allocator warnings mainId

addWarning :: PreprocessorWarning -> PreprocessorMonad ()
addWarning warning = do
    PreprocessorState scope allocator warnings mainId <- get
    put $ PreprocessorState scope allocator (warning:warnings) mainId
