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

existsScope :: ScopeType -> Parser Bool
existsScope scopeT = do
  scopeTypes <- fmap scopeType <$> gets scopes
  return (scopeT `elem` scopeTypes)

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

currentScopeIsGlobal :: Parser Bool
currentScopeIsGlobal = do
  sTypes <- fmap scopeType <$> gets scopes
  return $ N.head sTypes == ScopeTypeGlobal

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
addScope' sType Scope{scopeType=ScopeTypeGlobal} = modifyScopes $ N.cons (Scope sType [] M.empty newLocalVariables newLocalTemp)
addScope' sType Scope{scopeVariablesMemory=mVars, scopeTempMemory=mTemp} = modifyScopes $ N.cons (Scope sType [] M.empty mVars mTemp)

destroyScope :: Parser ()
destroyScope = do
  mScopes <- snd . N.uncons <$> gets scopes
  maybe (fail "Can't destroy all scopes") (\s -> modify(modifyScopes $ const s)) mScopes

scoped :: ScopeType -> Parser a -> Parser a
scoped sType = between (addScope sType) destroyScope

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
emptyClassDefinition father = ClassDefinition father [] (ClassConstructor [] Nothing []) M.empty

findClass :: Text -> Parser ClassDefinition
findClass cName = do
  cDefinitions <- gets classDefinitions
  maybeFail ("Class " ++ T.unpack cName ++ " doesn't exist") (M.lookup cName cDefinitions)

insertMemberToClass :: Text -> ClassMember -> ParserState -> ParserState
insertMemberToClass clsName member pState@ParserState{classDefinitions=cDefinitions} = pState{classDefinitions=M.update updateF clsName cDefinitions}
  where
    updateF cDef@ClassDefinition{classDefinitionMembers=members} = Just cDef{classDefinitionMembers=member:members}

insertMethodToClass :: Text -> Text -> FunctionDefinition -> ParserState -> ParserState
insertMethodToClass clsName methodName method pState@ParserState{classDefinitions=cDefinitions} = pState{classDefinitions=M.update updateF clsName cDefinitions}
  where
    updateF cDef@ClassDefinition{classDefinitionMethods=methods} = Just $ cDef{classDefinitionMethods=M.insert methodName method methods}

insertConstructorToClass :: Text -> ClassConstructor -> ParserState -> ParserState
insertConstructorToClass clsName constructor pState@ParserState{classDefinitions=cDefinitions} = pState{classDefinitions=M.update updateF clsName cDefinitions}
  where
    updateF cDef = Just cDef{classDefinitionConstructor=constructor}

registerObjectMembers :: ClassDefinition -> Text -> Parser ()
registerObjectMembers cls ident = do
  let clsMembers = classDefinitionMembers cls
  forM_ clsMembers (registerMember ident)

-- TODO: Review this when we get into more into objects
registerMember :: Text -> ClassMember -> Parser()
registerMember objectIdent (ClassMember memberIdent memberT) = modify $ insertVariable (Variable memberT True (Address (-1))) (memberKey objectIdent memberIdent)

setVariableAsInitialized :: Text -> ParserState -> ParserState
setVariableAsInitialized ident pState@ParserState{scopes=ss} = pState{scopes=updateF <$> ss}
  where
    updateF scope@Scope{scopeVariables=variables} = scope{scopeVariables=M.update f ident variables}
    f var = Just var{variableInit=True}

insideLoop :: Text -> Parser ()
insideLoop symbolName = do
  existsFor <- existsScope ScopeTypeFor
  existsWhile <- existsScope ScopeTypeWhile
  guardFail (existsFor || existsWhile) $ T.unpack symbolName ++ " must be inside a loop"
