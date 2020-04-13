{-# LANGUAGE RecordWildCards #-}

module Parser where

import           AnalysisUtils
import           Control.Monad.Combinators.Expr
import           Control.Monad.Combinators.NonEmpty
import           Data.List.NonEmpty                 (NonEmpty)
import qualified Data.List.NonEmpty                 as N
import           Lexer
import           ParserTypes
import           Text.Megaparsec                    hiding (sepBy1, some)
import qualified Text.Megaparsec.Char.Lexer         as L

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
      defSymbol
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

ifParser :: Parser Conditional
ifParser = do
  ifBlock <- indentBlock $ indentedCondition ifSymbol
  elifBlocks <- many $ indentBlock $ indentedCondition elifSymbol
  elseBlock <- optional $ indentBlock indentedElse
  return Conditional{..}
    where
      indentedCondition firstSymbol = do
        _ <- firstSymbol
        conditionalExpr <- expr
        colonSymbol
        indentSome (return . ConditionalBlock conditionalExpr) statement
      indentedElse = elseSymbol *> indentSome return statement

printParser :: Parser Statement
printParser = do
  printSymbol
  e <- parens expr
  return (PrintStatement e)

readParser :: Parser Statement
readParser = do
  ident <- identifier
  equalSymbol
  readSymbol
  -- TODO: reading int as a placeholder
  return (ReadStatement ident IntType)

declaration :: Parser Declaration
declaration = do
  letSymbol
  identifiers <- sepBy1 identifier commaSymbol
  colonSymbol
  idType <- composedType
  rExpr <- optional $ equalSymbol *> expr
  return (Declaration identifiers idType rExpr)

statement :: Parser Statement
statement = choice [ continueSymbol
                   , breakSymbol
                   , passSymbol
                   , ObjectAssignmentStatement <$> try objectAssignment
                   , ArrayAssignmentStatement <$> try arrayAssignmet
                   , SimpleAssignmentStatement <$> try simpleAssignment
                   , WhileStatement <$> whileParser
                   , ForLoopStatement <$> forParser
                   , ConditionalStatement <$> ifParser
                   , printParser
                   , DeclarationStatement <$> declaration
                   , readParser]

exprId :: Parser SimpleExpr
exprId = do
  ident <- identifier
  return (Var ident)

exprMemberAccess :: Parser SimpleExpr
exprMemberAccess = do
  obj <- selfSymbol <|> identifier
  dotSymbol
  member <- identifier
  return (MemberAccess obj member)

exprArrayAccess :: Parser SimpleExpr
exprArrayAccess = do
  ident <- identifier
  index <- brackets expr
  return (ArrayAccess ident index)

exprInt :: Parser SimpleExpr
exprInt = IntLiteral <$> intLiteral

exprFloat :: Parser SimpleExpr
exprFloat = FloatLiteral <$> floatLiteral

exprBool :: Parser SimpleExpr
exprBool = BoolLiteral <$> (trueSymbol <|> falseSymbol)

exprFunctionCall :: Parser SimpleExpr
exprFunctionCall = do
  functionName <- identifier
  arguments <- parens $ sepBy expr commaSymbol
  return (FunctionCall functionName arguments)

exprMethodCall :: Parser SimpleExpr
exprMethodCall = do
  objectName <- identifier
  dotSymbol
  methodName <- identifier
  arguments <- parens $ sepBy expr commaSymbol
  return (MethodCall objectName methodName arguments)

factor :: Parser SimpleExpr
factor = choice [ parens simpleExpr
                , try exprFloat
                , exprInt
                , exprBool
                , try exprMethodCall
                , try exprFunctionCall
                , try exprMemberAccess
                , try exprArrayAccess
                , exprId]

operatorTable :: [[Operator Parser SimpleExpr]]
operatorTable = [ [ prefix minusSymbol Neg
                  , prefix plusSymbol id]
                , [ rightBinary exponentSymbol]
                , [ binary timesSymbol
                  , binary divisionSymbol]
                , [ binary plusSymbol
                  , binary minusSymbol]
                , [ binary isEqualSymbol
                  , binary lessEqSymbol
                  , binary greaterEqSymbol
                  , binary differentSymbol
                  , binary lessSymbol
                  , binary greaterSymbol]
                , [ prefix notSymbol Not]
                , [ binary andSymbol]
                , [ binary orSymbol]]

binary :: Parser Op -> Operator Parser SimpleExpr
binary = InfixL . fmap Operate

rightBinary :: Parser Op -> Operator Parser SimpleExpr
rightBinary = InfixR . fmap Operate

prefix :: Parser a -> (SimpleExpr -> SimpleExpr) -> Operator Parser SimpleExpr
prefix name f = Prefix (f <$ name)

simpleExpr :: Parser SimpleExpr
simpleExpr = makeExprParser factor operatorTable

expr :: Parser Expr
expr = simpleExpr >>= exprCheck

simpleAssignment :: Parser SimpleAssignment
simpleAssignment = do
  i <- identifier
  equalSymbol
  e <- expr
  return (SimpleAssignment i e)

arrayAssignmet :: Parser ArrayAssignment
arrayAssignmet = do
  i <- identifier
  a <- brackets expr
  equalSymbol
  e <- expr
  return (ArrayAssignment i a e)

objectAssignment :: Parser ObjectAssignment
objectAssignment = do
  obj <- identifier
  dotSymbol
  member <- identifier
  equalSymbol
  e <- expr
  return (ObjectAssignment obj member e)

forParser :: Parser ForLoop
forParser = indentBlock forBlock
  where
    forBlock = do
      forSymbol
      forDeclaration <- sepBy1 identifier commaSymbol
      colonSymbol
      forDeclarationType <- simpleType
      equalSymbol
      forDeclarationExpr <- expr
      colonSymbol
      forCondition <- expr
      colonSymbol
      forAssigment <- simpleAssignment
      colonSymbol
      indentSome (return . ForLoop forDeclaration forDeclarationType forDeclarationExpr forCondition forAssigment) statement

classMember :: Parser ClassMember
classMember = do
  letSymbol
  memberIdentifier <- identifier
  colonSymbol
  memberType <- composedType
  return ClassMember{..}

classConstructorParameter :: Parser ClassConstructorParameter
classConstructorParameter = do
  classConstructorParameterId <- identifier
  colonSymbol
  classConstructorParemeterType <- composedType
  return ClassConstructorParameter{..}

classConstructorParser :: Parser ClassConstructor
classConstructorParser = indentBlock indentedConstructor
  where
    indentedConstructor = do
      _ <- identifier
      classConstructorParameters <- parens $ many classConstructorParameter
      classSuperConstructor <- optional . try $ colonSymbol *> superConstructor
      colonSymbol
      indentSome (return . ClassConstructor classConstructorParameters classSuperConstructor) constructorAssignments
    superConstructor = do
      superSymbol
      parens $ many identifier
    constructorAssignments = choice [ ConstructorPass <$ passSymbol
                                    , ConstructorSimpleAssignment <$> try simpleAssignment
                                    , ConstructorObjectAssignment <$> try objectAssignment
                                    , ConstructorArrayAssignment <$> try arrayAssignmet]
