module AnalysisUtils where

import           ParserTypes
import qualified Data.Map.Strict as M
import           Data.Text                          (Text)
import           Data.Foldable
import           Control.Monad.State.Lazy 
import qualified Data.List.NonEmpty as N


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

addPlaceholderVariable :: Text -> ParserState -> ParserState
addPlaceholderVariable identifier (ParserState ((Scope sType sVariables) N.:| scopes) x y) = ParserState ((Scope sType (M.insert identifier (Variable VariablePlaceholder False) sVariables)) N.:| scopes) x y 