{-# LANGUAGE RecordWildCards #-}

module Parser where

import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as N
import           Lexer
import           ParserTypes
import           Text.Megaparsec            hiding (some)
import qualified Text.Megaparsec.Char.Lexer as L

indentBlock :: Parser (IndentOpt a b) -> Parser a
indentBlock = L.indentBlock scn

indentation :: Maybe Pos
indentation = Just (mkPos 3)

indentSome :: (NonEmpty b -> Parser a) -> Parser b -> Parser (IndentOpt a b)
indentSome f = return . L.IndentSome indentation (f . N.fromList)

simpleType :: Parser SimpleType
simpleType = choice [intSymbol, boolSymbol, floatSymbol, stringSymbol, charSymbol]

composedType :: Parser ComposedType
composedType = try (ArrayType <$> simpleType <*> brackets intLiteral) <|> Simple <$> simpleType <|> ClassType <$> identifier

returnType :: Parser ReturnType
returnType = voidSymbol <|> ValueReturn <$> simpleType

functionArgument :: Parser FunctionArgument
functionArgument = do
  argumentName <- identifier
  colonSymbol
  argumentType <- simpleType
  return FunctionArgument{..}

functionParser :: Parser Function
functionParser = indentBlock functionBlock
  where
    functionBlock = do
      functionName <- identifier
      functionArguments <- parens $ sepBy functionArgument commaSymbol
      arrowSymbol
      functionReturnType <- returnType
      colonSymbol
      indentSome (return . Function functionName functionArguments functionReturnType) statement

whileParser :: Parser WhileLoop
whileParser = indentBlock whileBlock
  where
    whileBlock = do
      whileSymbol
      whileCondition <- expr
      colonSymbol
      indentSome (return . WhileLoop whileCondition) statement

statement :: Parser Statement
statement = choice [ continueSymbol
                   , breakSymbol
                   , passSymbol]

exprId :: Parser Expr
exprId = do
  ident <- identifier
  return (Var ident)

exprInt :: Parser Expr
exprInt = I <$> intLiteral

exprFloat :: Parser Expr
exprFloat = F <$> floatLiteral

expr :: Parser Expr
expr = undefined
