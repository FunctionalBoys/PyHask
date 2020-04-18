module AnalysisUtils where

import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as N
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import           ParserTypes


--TODO: Actually do this
exprCheck :: SimpleExpr -> Parser Expr
exprCheck e = return $ Expr e (Simple IntType)

existsIdentifier :: Text -> Parser Bool
existsIdentifier identifier = do
    c <- existsClass identifier
    f <- existsFunction identifier
    v <- existsVariable identifier
    return (c || f || v)

existsClass :: Text -> Parser Bool
existsClass classIdentifier = do
    definitions <- gets classDefinitions
    return $ M.member classIdentifier definitions

existsFunction :: Text -> Parser Bool
existsFunction functionIdentifier = do
    definitions <- gets functionDefinitions
    return $ M.member functionIdentifier definitions

existsVariable :: Text -> Parser Bool
existsVariable variableIdentifier = do
    list <- gets scopes
    return (any (M.member variableIdentifier) (fmap scopeVariables list))

modifyScopes :: (NonEmpty Scope -> NonEmpty Scope) -> ParserState -> ParserState
modifyScopes f (ParserState s d c) = ParserState (f s) d c

addPlaceHolderToScope :: Text -> Scope -> Scope
addPlaceHolderToScope identifier (Scope sType sVariables) = Scope sType (M.insert identifier (Variable (Simple IntType) False) sVariables)

addPlaceholderVariable :: Text -> ParserState -> ParserState
addPlaceholderVariable identifier (ParserState (scope N.:| rest) x y) = ParserState (addPlaceHolderToScope identifier scope N.:| rest) x y

addPlaceHolderFunction :: Text -> ParserState -> ParserState
addPlaceHolderFunction identifier (ParserState s fDefinitions c) = ParserState s (M.insert identifier (FunctionDefinition [] (ValueReturn IntType)) fDefinitions) c

scoped :: ScopeType -> Parser a -> Parser a
scoped sType p = do
  modify (modifyScopes $ N.cons (Scope sType M.empty))
  result <- p
  mScopes <- snd. N.uncons <$> gets scopes
  maybe (fail "Can't destroy all scopes") (\s -> modify(modifyScopes $ const s)) mScopes
  return result
