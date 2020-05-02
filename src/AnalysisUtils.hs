{-# LANGUAGE OverloadedStrings #-}

module AnalysisUtils where

import           Control.Monad.Combinators
import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as N
import qualified Data.Map.Strict           as M
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           ParserTypes


getValueReturn :: ReturnType -> Parser ComposedType
getValueReturn (ValueReturn sType) = return (Simple sType)
getValueReturn _ = fail "Only functions that return values can be used in expression."

exprSimpleType :: Expr -> Parser Expr
exprSimpleType (Expr sExpr (Simple sType)) = return (Expr sExpr (Simple sType))
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
addIdentifier ident = modifyScope (\(Scope st ids v) -> Scope st (ident:ids) v)

maybeToParser :: String -> Maybe a -> Parser a
maybeToParser e = maybe (fail e) return

guardFail :: Bool -> String -> Parser ()
guardFail True _  = return ()
guardFail False s = fail s

findVariable :: Text -> Parser Variable
findVariable ident = do
  maps <- fmap scopeVariables <$> gets scopes
  maybeToParser ("Variable " ++ T.unpack ident ++ " not found") $ asum $ fmap (M.lookup ident) maps

getArrayInfoFromType :: ComposedType -> Parser (SimpleType, Int)
getArrayInfoFromType (ArrayType sType sz) = return (sType, sz)
getArrayInfoFromType _                    = fail "Type is not an array"

extractSimpleType :: ComposedType -> Parser SimpleType
extractSimpleType (Simple sType) = return sType
extractSimpleType _              = fail "Type was not simple"

extractClassName :: ComposedType -> Parser Text
extractClassName (ClassType clsName) = return clsName
extractClassName _                   = fail "Type is not a class"

getArrayInfo :: Text -> Parser (SimpleType, Int)
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
  maybeToParser ("Function " ++ T.unpack fName ++ " doesn't exist") (M.lookup fName fDefinitions)

findScopeFunctionName :: Parser Text
findScopeFunctionName = do
  sTypes <- fmap scopeType <$> gets scopes
  maybeToParser "No parent function" $ asum $ fmap f sTypes
  where
    f (ScopeTypeFunction fName) = Just fName
    f _                         = Nothing

maybeClassName :: ScopeType -> Maybe Text
maybeClassName (ScopeTypeClass name) = Just name
maybeClassName _                     = Nothing

findScopeClassName :: Parser Text
findScopeClassName = do
  sTypes <- gets $ fmap scopeType . scopes
  maybeToParser "Not inside class" $ asum $ fmap maybeClassName sTypes

maybeInsideClass :: Parser (Maybe Text)
maybeInsideClass = do
  sTypes <- gets $ fmap scopeType . scopes
  return $ asum $ fmap maybeClassName sTypes

modifyScopes :: (NonEmpty Scope -> NonEmpty Scope) -> ParserState -> ParserState
modifyScopes f (ParserState s d c q) = ParserState (f s) d c q

modifyScope :: (Scope -> Scope) -> ParserState -> ParserState
modifyScope f (ParserState (s N.:| ss) d c q) = ParserState (f s N.:| ss) d c q

addScope :: ScopeType -> Parser ()
addScope sType = modify (modifyScopes $ N.cons (Scope sType [] M.empty))

destroyScope :: Parser ()
destroyScope = do
  mScopes <- snd . N.uncons <$> gets scopes
  maybe (fail "Can't destroy all scopes") (\s -> modify(modifyScopes $ const s)) mScopes

scoped :: ScopeType -> Parser a -> Parser a
scoped sType = between (addScope sType) destroyScope

createVariable :: ComposedType -> Maybe Expr -> Variable
createVariable vType expr = Variable vType (isJust expr)

insertVariable :: Variable -> Text -> ParserState -> ParserState
insertVariable v ident  = modifyScope (\(Scope sType ids variables) -> Scope sType ids (M.insert ident v variables))

insertFunction :: Text -> FunctionDefinition -> ParserState -> ParserState
insertFunction ident f (ParserState s fDefinitions c q) = ParserState s (M.insert ident f fDefinitions) c q

insertClassDefinition :: Text -> ClassDefinition -> ParserState -> ParserState
insertClassDefinition ident cls (ParserState s fs cDefinitions q) = ParserState s fs (M.insert ident cls cDefinitions) q

emptyClassDefinition :: Maybe Text -> ClassDefinition
emptyClassDefinition father = ClassDefinition father [] (ClassConstructor [] Nothing []) M.empty

findClass :: Text -> Parser ClassDefinition
findClass cName = do
  cDefinitions <- gets classDefinitions
  maybeToParser ("Class " ++ T.unpack cName ++ " doesn't exist") (M.lookup cName cDefinitions)

insertMemberToClass :: Text -> ClassMember -> ParserState -> ParserState
insertMemberToClass clsName member (ParserState s f cDefinitions q) = ParserState s f (M.update updateF clsName cDefinitions) q
  where
    updateF (ClassDefinition father members constructor methods) = Just (ClassDefinition father (member:members) constructor methods)

insertMethodToClass :: Text -> Text -> FunctionDefinition -> ParserState -> ParserState
insertMethodToClass clsName methodName method (ParserState s f cDefinitions q) = ParserState s f (M.update updateF clsName cDefinitions) q
  where
    updateF (ClassDefinition father members constructor methods) = Just (ClassDefinition father members constructor (M.insert methodName method methods))

insertConstructorToClass :: Text -> ClassConstructor -> ParserState -> ParserState
insertConstructorToClass clsName constructor (ParserState s f cDefinitions q) = ParserState s f (M.update updateF clsName cDefinitions) q
  where
    updateF (ClassDefinition father members _ methods) = Just (ClassDefinition father members constructor methods)

registerObjectMembers :: ClassDefinition -> Text -> Parser ()
registerObjectMembers cls ident = do
  let clsMembers = classDefinitionMembers cls
  forM_ clsMembers (registerMember ident)

registerMember :: Text -> ClassMember -> Parser()
registerMember objectIdent (ClassMember memberIdent memberT) = modify $ insertVariable (Variable memberT True) (memberKey objectIdent memberIdent)

setVariableAsInitialized :: Text -> ParserState -> ParserState
setVariableAsInitialized ident (ParserState ss fs cs qs) = ParserState (updateF <$> ss) fs cs qs
  where
    updateF (Scope sT sI sVariables) = Scope sT sI (M.update f ident sVariables)
    f (Variable vT _) = Just (Variable vT True)

insideLoop :: Text -> Parser ()
insideLoop symbolName = do
  existsFor <- existsScope ScopeTypeFor
  existsWhile <- existsScope ScopeTypeWhile
  if existsFor || existsWhile
    then return ()
    else fail $ T.unpack symbolName ++ " must be inside a loop"
