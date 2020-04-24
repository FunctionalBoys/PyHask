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


--TODO: Actually do this
exprCheck :: SimpleExpr -> Parser Expr
exprCheck (Var ident) = do
  (Variable vType initialized) <- findVariable ident
  if initialized
    then fail $ "Can't use uninitailized variable " ++ T.unpack ident
  else
    return (Expr (Var ident) vType)
exprCheck (IntLiteral integer) = return (Expr (IntLiteral integer) (Simple IntType))
exprCheck (FloatLiteral float) = return (Expr (FloatLiteral float) (Simple FloatType))
exprCheck (BoolLiteral bool) = return (Expr (BoolLiteral bool) (Simple BoolType))
exprCheck (StringLiteral sLiteral) = return (Expr (StringLiteral sLiteral) (Simple StringType))
exprCheck (Not sExpr) = do
  (Expr cExpr cType) <- exprCheck sExpr
  if cType == Simple BoolType
    then return (Expr (Not cExpr) cType)
    else fail "Only boolean expressions can be negated"
exprCheck (Neg sExpr) = do
  (Expr cExpr cType) <- exprCheck sExpr
  if cType == Simple IntType || cType == Simple FloatType
    then return (Expr (Neg cExpr) cType)
    else fail "Only numeric values have a negative"
exprCheck (ArrayAccess ident (Expr (IntLiteral integer) (Simple IntType))) = do
  array <- findArray ident
  if integer >= arraySize array
    then fail "Index out of bounds"
    else return (Expr (ArrayAccess ident (Expr (IntLiteral integer) (Simple IntType))) (Simple (arrayType array)))
exprCheck (ArrayAccess ident (Expr sExpr (Simple IntType))) = do
  array <- findArray ident
  return (Expr (ArrayAccess ident (Expr sExpr (Simple IntType))) (Simple (arrayType array)))
exprCheck (ArrayAccess _ _) = fail "Array index must be of integral type"
exprCheck e = return $ Expr e (Simple IntType)

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
addIdentifier ident = modifyScope (\(Scope st ids v a) -> Scope st (ident:ids) v a)

maybeToParser :: String -> Maybe a -> Parser a
maybeToParser e = maybe (fail e) return

findVariable :: Text -> Parser Variable
findVariable ident = do
  maps <- fmap scopeVariables <$> gets scopes
  maybeToParser ("Variable " ++ T.unpack ident ++ " not found") $ asum $ fmap (M.lookup ident) maps

existsScope :: ScopeType -> Parser Bool
existsScope scopeT = do
  scopeTypes <- fmap scopeType <$> gets scopes
  return (scopeT `elem` scopeTypes)

findArray :: Text -> Parser Array
findArray ident = do
  maps <- fmap scopeArrays <$> gets scopes
  maybeToParser ("Array " ++ T.unpack ident ++ " not found") $ asum $ fmap (M.lookup ident) maps

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
modifyScopes f (ParserState s d c) = ParserState (f s) d c

modifyScope :: (Scope -> Scope) -> ParserState -> ParserState
modifyScope f (ParserState (s N.:| ss) d c) = ParserState (f s N.:| ss) d c

addScope :: ScopeType -> Parser ()
addScope sType = modify (modifyScopes $ N.cons (Scope sType [] M.empty M.empty))

destroyScope :: Parser ()
destroyScope = do
  mScopes <- snd . N.uncons <$> gets scopes
  maybe (fail "Can't destroy all scopes") (\s -> modify(modifyScopes $ const s)) mScopes

scoped :: ScopeType -> Parser a -> Parser a
scoped sType = between (addScope sType) destroyScope

createVariable :: ComposedType -> Maybe Expr -> Variable
createVariable vType expr = Variable vType (isJust expr)

createArray :: SimpleType -> Int -> Maybe Expr -> Array
createArray aType sz expr = Array aType sz (isJust expr)

insertVariable :: Variable -> Text -> ParserState -> ParserState
insertVariable v ident  = modifyScope (\(Scope sType ids variables arrays) -> Scope sType ids (M.insert ident v variables) arrays)

insertArray :: Array -> Text -> ParserState -> ParserState
insertArray a ident = modifyScope (\(Scope sType ids variables arrays) -> Scope sType ids variables (M.insert ident a arrays))

insertFunction :: Text -> FunctionDefinition -> ParserState -> ParserState
insertFunction ident f (ParserState s fDefinitions c) = ParserState s (M.insert ident f fDefinitions) c

insertClassDefinition :: Text -> ClassDefinition -> ParserState -> ParserState
insertClassDefinition ident cls (ParserState s fs cDefinitions) = ParserState s fs (M.insert ident cls cDefinitions)

emptyClassDefinition :: Maybe Text -> ClassDefinition
emptyClassDefinition father = ClassDefinition father [] (ClassConstructor [] Nothing []) []

findClass :: Text -> Parser ClassDefinition
findClass cName = do
  cDefinitions <- gets classDefinitions
  maybeToParser ("Class " ++ T.unpack cName ++ " doesn't exist") (M.lookup cName cDefinitions)

insertMemberToClass :: Text -> ClassMember -> ParserState -> ParserState
insertMemberToClass clsName member (ParserState s f cDefinitions) = ParserState s f (M.update updateF clsName cDefinitions)
  where
    updateF (ClassDefinition father members constructor methods) = Just (ClassDefinition father (member:members) constructor methods)

insertMethodToClass :: Text -> FunctionDefinition -> ParserState -> ParserState
insertMethodToClass clsName method (ParserState s f cDefinitions) = ParserState s f (M.update updateF clsName cDefinitions)
  where
    updateF (ClassDefinition father members constructor methods) = Just (ClassDefinition father members constructor (method:methods))

insertConstructorToClass :: Text -> ClassConstructor -> ParserState -> ParserState
insertConstructorToClass clsName constructor (ParserState s f cDefinitions) = ParserState s f (M.update updateF clsName cDefinitions)
  where
    updateF (ClassDefinition father members _ methods) = Just (ClassDefinition father members constructor methods)
