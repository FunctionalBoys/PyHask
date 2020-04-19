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
exprCheck e = return $ Expr e (Simple IntType)

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
  return (elem scopeT scopeTypes)

modifyScopes :: (NonEmpty Scope -> NonEmpty Scope) -> ParserState -> ParserState
modifyScopes f (ParserState s d c) = ParserState (f s) d c

modifyScope :: (Scope -> Scope) -> ParserState -> ParserState
modifyScope f (ParserState (s N.:| ss) d c) = ParserState (f s N.:| ss) d c

addPlaceHolderFunction :: Text -> ParserState -> ParserState
addPlaceHolderFunction identifier = insertFunction identifier (FunctionDefinition [] (ValueReturn IntType))

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

insertVariable :: Variable -> Text -> ParserState -> ParserState
insertVariable v ident  = modifyScope (\(Scope sType ids variables arrays) -> Scope sType ids (M.insert ident v variables) arrays)

insertFunction :: Text -> FunctionDefinition -> ParserState -> ParserState
insertFunction ident f (ParserState s fDefinitions c) = ParserState s (M.insert ident f fDefinitions) c
