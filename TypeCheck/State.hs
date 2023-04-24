module TypeCheck.State where
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (isNothing, Maybe (..), fromJust, isJust)
import Data.Map (Map, empty, fromList, insert, lookup)
import Fe.Abs (Ident (..), BNFC'Position)
import Common.Utils (Identifier, Identifier' (..))
import Common.Types
import TypeCheck.Error
import Prelude hiding (lookup)

data Scope a =
    Global a |
    Local (Scope a) a
  deriving (Eq, Ord, Show, Read)

isGlobal :: Scope a -> Bool
isGlobal (Global _) = True
isGlobal (Local _ _) = False

isLocal :: Scope a -> Bool
isLocal (Global _) = False
isLocal (Local _ _) = True

data VariableState =
    Uninitialized |
    Borrowed Integer BNFC'Position |
    BorrowedMut BNFC'Position |
    Free
  deriving (Eq, Ord, Show, Read)

data Variable = Variable Ident Type VariableState
  deriving (Eq, Ord, Show, Read)

type VariableMappings = Map Ident VariableId
type TypeDefinitions = Map Ident Type
type PreprocessorScope = Scope (VariableMappings, TypeDefinitions)

type VariableId = Int
data Allocator = Allocator [Variable] [VariableId]
  deriving (Eq, Ord, Show, Read)

data PreprocessorState = PreprocessorState PreprocessorScope Allocator
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
    (Allocator [] [])

getType :: Identifier -> PreprocessorMonad Type
getType identifier = do
    let (Identifier p ident) = identifier
    PreprocessorState scope _ <- get
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
    PreprocessorState scope allocator <- get
    let maybeDefinition = getTypeLocal identifier scope
    when (isJust maybeDefinition) $ do throwError $ TypeAlreadyInScope identifier t (fromJust maybeDefinition)
    put $ PreprocessorState
        (helper ident t scope)
        allocator
  where
    helper ident t (Global (variables, types)) = Global (variables, insert ident t types)
    helper ident t (Local parent (variables, types)) = Local parent (variables, insert ident t types)

getVariable :: Identifier -> PreprocessorMonad Variable
getVariable identifier = do
    let (Identifier _ ident) = identifier
    PreprocessorState scope (Allocator variableStack _) <- get
    let maybeId = helper ident scope
    when (isNothing maybeId) $ do throwError $ VariableNotDefined identifier
    return $ variableStack !! fromJust maybeId
  where
    helper ident (Global (variables, _)) = lookup ident variables
    helper ident (Local parent (variables, _)) =
        let x = lookup ident variables in
        if isNothing x then helper ident parent
        else x

addVariable :: Identifier -> Variable -> PreprocessorMonad ()
addVariable identifier variable = do
    let (Identifier _ ident) = identifier
    PreprocessorState scope allocator <- get
    let (id, allocator') = allocate allocator variable
    put $ PreprocessorState
        (helper ident id scope)
        allocator'
  where
    helper ident id (Global (variables, types)) = Global (insert ident id variables, types)
    helper ident id (Local parent (variables, types)) = Local parent (insert ident id variables, types)


allocate :: Allocator -> Variable -> (VariableId, Allocator)
allocate (Allocator variableStack freeCells) variable =
    case freeCells of
        [] -> (length variableStack, Allocator (variableStack++[variable]) [])
        (h:t) ->
            let (pref, _:suf) = splitAt h variableStack in
            (h, Allocator (pref++(variable:suf)) t)

free :: Allocator -> VariableId -> Allocator
free (Allocator variableStack freeCells) id =
    if length variableStack == id then
        Allocator (take (id - 1) variableStack) freeCells
    else
        Allocator variableStack (id:freeCells)
