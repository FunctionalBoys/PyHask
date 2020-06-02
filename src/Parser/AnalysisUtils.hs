{-# LANGUAGE OverloadedStrings #-}

module Parser.AnalysisUtils where

import           Control.Monad.Combinators
import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as N
import qualified Data.Map.Strict           as M
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Parser.ParserTypes
import           Parser.Utils


getValueReturn :: ReturnType -> Parser ComposedType
getValueReturn (ValueReturn sType) = return (Simple sType)
getValueReturn _ = fail "Only functions that return values can be used in expression."

getValueReturnSimple :: ReturnType -> Parser SimpleType
getValueReturnSimple (ValueReturn sType) = return sType
getValueReturnSimple _                   = fail "Function has void as return"

exprSimpleType :: Expr -> Parser Expr
exprSimpleType sExpr@Expr{expressionType=Simple _} = return sExpr
exprSimpleType _ = fail "Expected a simple type"

existsIdentifier :: Text -> Parser Bool
existsIdentifier identifier = do
    c <- existsClass identifier
    f <- existsFunction identifier
    s <- existsInScope identifier
    return (c || f || s)

existsClass :: Text -> Parser Bool
existsClass classIdentifier = do
    definitions <- gets classDefinitions
    return $ M.member classIdentifier definitions

existsFunction :: Text -> Parser Bool
existsFunction functionIdentifier = do
    definitions <- gets functionDefinitions
    return $ M.member functionIdentifier definitions

existsInScope :: Text -> Parser Bool
existsInScope variableIdentifier = do
    list <- gets scopes
    return $ any (elem variableIdentifier) (fmap scopeIdentifiers list)

addIdentifier :: Text -> ParserState -> ParserState
addIdentifier ident = modifyScope f
  where
    f scope@Scope{scopeIdentifiers=ids} = scope{scopeIdentifiers=ident:ids}

findVariable :: Text -> Parser Variable
findVariable ident = do
  maps <- fmap scopeVariables <$> gets scopes
  maybeFail ("Variable " ++ T.unpack ident ++ " not found") $ asum $ fmap (M.lookup ident) maps

getArrayInfoFromType :: ComposedType -> Parser (SimpleType, NonEmpty Int)
getArrayInfoFromType (ArrayType sType sz) = return (sType, sz)
getArrayInfoFromType _                    = fail "Type is not an array"

extractSimpleType :: ComposedType -> Parser SimpleType
extractSimpleType (Simple sType) = return sType
extractSimpleType _              = fail "Type was not simple"

extractClassName :: ComposedType -> Parser Text
extractClassName (ClassType clsName) = return clsName
extractClassName _                   = fail "Type is not a class"

getArrayInfo :: Text -> Parser (SimpleType, NonEmpty Int)
getArrayInfo ident = (variableType <$> findVariable ident) >>= getArrayInfoFromType

memberKey :: Text -> Text -> Text
memberKey obj member = obj <> "." <> member

existsScope :: (ScopeType -> Bool) -> Parser Bool
existsScope scopeT = do
  scopeTypes <- fmap scopeType <$> gets scopes
  return (any scopeT scopeTypes)

findFunction :: Text -> Parser FunctionDefinition
findFunction fName = do
  fDefinitions <- gets functionDefinitions
  maybeFail ("Function " ++ T.unpack fName ++ " doesn't exist") (M.lookup fName fDefinitions)

findScopeFunctionName :: Parser Text
findScopeFunctionName = do
  sTypes <- fmap scopeType <$> gets scopes
  maybeFail "No parent function" $ asum $ fmap f sTypes
  where
    f (ScopeTypeFunction fName) = Just fName
    f _                         = Nothing

-- TODO: This is gonna have to change for objects too
currentScopeIsGlobal :: Parser Bool
currentScopeIsGlobal = do
  sTypes <- fmap scopeType <$> gets scopes
  return $ all f sTypes
  where
    f (ScopeTypeFunction _) = False
    f ScopePlaceholder = False
    f _ = True

insideFunction :: Parser Bool
insideFunction = do
  sTypes <- fmap scopeType <$> gets scopes
  return (any f sTypes)
  where
    f (ScopeTypeFunction _) = True
    f _                     = False

maybeClassName :: ScopeType -> Maybe Text
maybeClassName (ScopeTypeClass name) = Just name
maybeClassName _                     = Nothing

findScopeClassName :: Parser Text
findScopeClassName = do
  sTypes <- gets $ fmap scopeType . scopes
  maybeFail "Not inside class" $ asum $ fmap maybeClassName sTypes

maybeInsideClass :: Parser (Maybe Text)
maybeInsideClass = do
  sTypes <- gets $ fmap scopeType . scopes
  return $ asum $ fmap maybeClassName sTypes

modifyScopes :: (NonEmpty Scope -> NonEmpty Scope) -> ParserState -> ParserState
modifyScopes f pState@ParserState{scopes=ss} = pState{scopes=f ss}

modifyScope :: (Scope -> Scope) -> ParserState -> ParserState
modifyScope f pState@ParserState{scopes=s N.:| ss} = pState{scopes = f s N.:| ss}

addScope :: ScopeType -> Parser ()
addScope sType = do
  currentScope <- gets $ N.head . scopes
  modify $ addScope' sType currentScope

addScope' :: ScopeType -> Scope -> (ParserState -> ParserState)
addScope' ScopePlaceholder _ = modifyScopes $ N.cons (Scope ScopePlaceholder [] M.empty newLocalVariables newLocalTemp)
addScope' sType Scope{scopeVariablesMemory=mVars, scopeTempMemory=mTemp} = modifyScopes $ N.cons (Scope sType [] M.empty mVars mTemp)

destroyScope :: Parser ScopeType
destroyScope = do
  (popped, mScopes) <- N.uncons <$> gets scopes
  maybe (fail "Can't destroy all scopes") (\s -> modify(modifyScopes $ const s)) mScopes
  return $ scopeType popped

scoped :: ScopeType -> Parser a -> Parser a
scoped sType = between (addScope sType) destroyScope

dataScoped :: ScopeType -> Parser a -> Parser (a,ScopeType)
dataScoped sType parser = do
  addScope sType
  result <- parser
  scopedData <- destroyScope
  return (result, scopedData)

createVariable :: ComposedType -> Maybe Expr -> Address -> Variable
createVariable vType expr = Variable vType (isJust expr)

insertVariable :: Variable -> Text -> ParserState -> ParserState
insertVariable v ident  = modifyScope f
  where
    f scope@Scope{scopeVariables=variables} = scope {scopeVariables=M.insert ident v variables}

insertGlobalVariable :: Variable -> Text -> ParserState -> ParserState
insertGlobalVariable var ident pState@ParserState{scopes = ss} = pState{scopes = f <$> ss}
  where
    f scope@Scope{scopeType=ScopeTypeGlobal,scopeVariables=vars} = scope{scopeVariables = M.insert ident var vars}
    f scope = scope

insertFunction :: Text -> FunctionDefinition -> ParserState -> ParserState
insertFunction ident f pState@ParserState{functionDefinitions=fDefinitions} = pState{functionDefinitions=M.insert ident f fDefinitions}

insertClassDefinition :: Text -> ClassDefinition -> ParserState -> ParserState
insertClassDefinition ident cls pState@ParserState{classDefinitions=cDefinitions} = pState{classDefinitions=M.insert ident cls cDefinitions}

emptyClassDefinition :: Maybe Text -> ClassDefinition
emptyClassDefinition father = ClassDefinition father [] (ClassConstructor [] Nothing [])

findClass :: Text -> Parser ClassDefinition
findClass cName = do
  cDefinitions <- gets classDefinitions
  maybeFail ("Class " ++ T.unpack cName ++ " doesn't exist") (M.lookup cName cDefinitions)

insertMemberToClass :: Text -> ClassMember -> ParserState -> ParserState
insertMemberToClass clsName member pState@ParserState{classDefinitions=cDefinitions} = pState{classDefinitions=M.update updateF clsName cDefinitions}
  where
    updateF cDef@ClassDefinition{classDefinitionMembers=members} = Just cDef{classDefinitionMembers=member:members}

insertConstructorToClass :: Text -> ClassConstructor -> ParserState -> ParserState
insertConstructorToClass clsName constructor pState@ParserState{classDefinitions=cDefinitions} = pState{classDefinitions=M.update updateF clsName cDefinitions}
  where
    updateF cDef = Just cDef{classDefinitionConstructor=constructor}

setVariableAsInitialized :: Text -> ParserState -> ParserState
setVariableAsInitialized ident pState@ParserState{scopes=ss} = pState{scopes=updateF <$> ss}
  where
    updateF scope@Scope{scopeVariables=variables} = scope{scopeVariables=M.update f ident variables}
    f var = Just var{variableInit=True}

insideLoop :: Text -> Parser ()
insideLoop symbolName = do
  existsFor <- existsScope forCheck
  existsWhile <- existsScope whileCheck
  guardFail (existsFor || existsWhile) $ T.unpack symbolName ++ " must be inside a loop"
  where
    forCheck (ScopeTypeFor _ _) = True
    forCheck _                  = False
    whileCheck (ScopeTypeWhile _ _) = True
    whileCheck _                    = False

findMember :: Text -> ClassMember -> Bool
findMember name ClassMember{memberIdentifier=ident} = name == ident

getMemberType :: Text -> ComposedType -> Parser SimpleType
getMemberType memberName (ClassType clsName) = do
  ClassDefinition{classDefinitionMembers=members} <- findClass clsName
  let nMember = find (findMember memberName) members
  maybe (fail $ "No member with name " <> T.unpack memberName) (return . memberType) nMember
getMemberType _ _ = fail "Variable type is not an object"
