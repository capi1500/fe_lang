module TypeCheck.VariablesUtils where

import Common.Utils
import TypeCheck.State
import TypeCheck.Variable
import Fe.Abs (Ident (Ident), BNFC'Position)
import Common.Scope
import Common.Types
import Data.Maybe
import Control.Monad.State
import Data.Map
import Prelude hiding (lookup)
import TypeCheck.Error
import TypeCheck.LifetimeUtils
import TypeCheck.Printer

getVariable :: Identifier -> PreprocessorMonad Variable
getVariable ident = do
    p <- gets position
    Variables scope variables <- gets variables
    let maybeOut = helper ident scope variables
    when (isNothing maybeOut) $ throw (VariableNotDefined p ident)
    return $ fromJust maybeOut
  where
    helper ident (Global mappings) variables = do
        id <- lookup ident mappings
        return $ listGet id variables
    helper ident (Local parent mappings) variables = do
        let id = lookup ident mappings
        if isNothing id then do
            helper ident parent variables
        else do
            let id' = fromJust id
            return $ listGet id' variables

getLocalVariable :: Identifier -> PreprocessorMonad (Maybe (VariableId, Variable))
getLocalVariable ident = do
    variables <- gets variables
    return $ helper ident variables
  where
    helper ident (Variables (Global mappings) variables) = do
        id <- lookup ident mappings
        return (id, listGet id variables)
    helper ident (Variables (Local parent mappings) variables) = do
        id <- lookup ident mappings
        return (id, listGet id variables)

isGlobalVariable :: Identifier -> PreprocessorMonad Bool
isGlobalVariable ident = do
    Variables scope variables <- gets variables
    return $ helper ident scope variables
  where
    helper :: Identifier -> VariableMappings -> [Variable] -> Bool
    helper ident (Global mappings) variables =
        isJust (lookup ident mappings)
    helper ident (Local parent mappings) variables =
        let id = lookup ident mappings in
        isNothing id && helper ident parent variables

-- does not change lifetimes, takes it from the environment
addVariable :: Identifier -> Mutable -> Value -> PreprocessorMonad VariableId
addVariable ident mut value = do
    id <- checkShadowing ident
    internalAddVariable (Just ident) mut value Free id

-- does not change lifetimes, takes it from the environment
addTemporaryVariable :: Mutable -> Value -> PreprocessorMonad VariableId
addTemporaryVariable mut value = do
    internalAddVariable Nothing mut value Free Nothing

internalAddVariable :: Maybe Identifier -> Mutable -> Value -> VariableState -> Maybe VariableId -> PreprocessorMonad VariableId
internalAddVariable identifier mut value variableState id = do
    p <- gets position
    variables <- gets variables
    lifetime <- getLifetime
    let variable = Variable {
        variableCreatedAt = p,
        variableName = identifier,
        variableId = -1,
        variableMutability = mut,
        variableType = valueType value,
        variableState = variableState,
        variableValue = value,
        lifetime = lifetime
    }
    helper identifier variable variables id
  where
    helper ident variable (Variables (Global mappings) variables) maybeId = do
        state <- get
        let (id, variables') = if isJust maybeId then
                let id = fromJust maybeId in
                let variable' = setVariableId id variable in
                (id, listSet id variable' variables)
            else
                let id = length variables in
                let variable' = setVariableId id variable in
                (id, listPushBack variable' variables)
        putVariables $ Variables (Global (maybeAddMapping ident id mappings)) variables'
        return id
    helper ident variable (Variables (Local parent mappings) variables) maybeId = do
        state <- get
        let id = fromMaybe (length variables) maybeId
        let variable' = setVariableId id variable
        putVariables $ Variables (Local parent (maybeAddMapping ident id mappings)) (listPushBack variable' variables)
        return id
    maybeAddMapping (Just ident) id mappings = insert ident id mappings
    maybeAddMapping _ _ mappings = mappings

getVariableById :: VariableId -> PreprocessorMonad Variable
getVariableById id = do
    Variables scope variables <- gets variables
    return $ listGet id variables

setVariableById :: VariableId -> Variable -> PreprocessorMonad ()
setVariableById id variable = do
    Variables mappings variables <- gets variables
    let variables' = listSet id variable variables
    putVariables $ Variables mappings variables'

mutateVariableById :: VariableId -> (Variable -> Variable) -> PreprocessorMonad ()
mutateVariableById id mutate = do
    variable <- getVariableById id
    setVariableById id (mutate variable)

checkShadowing :: String -> PreprocessorMonad (Maybe VariableId)
checkShadowing name = do
    maybeVar <- getLocalVariable name
    return $ do
        (i, _) <- maybeVar
        Just i
  where
    shadowingOk (_, Variable _ _ t _ _ variableState _ _) =
        isFunction t && variableState == Uninitialized
