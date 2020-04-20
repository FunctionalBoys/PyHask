{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import           AnalysisUtils
import           Control.Monad.Combinators.Expr
import           Control.Monad.Combinators.NonEmpty
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Default.Class
import           Data.List.NonEmpty                 (NonEmpty)
import qualified Data.List.NonEmpty                 as N
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Lexer
import           ParserTypes
import           Text.Megaparsec                    hiding (sepBy1)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

mainParser :: Parser MainProgram
mainParser = do
  mainProgramDefinitions <- many $ choice $ fmap nonIndented [ MainProgramFunction <$> functionParser <?> "function definition"
                                                             , MainProgramDeclaration <$> declaration <?> "global variable"
                                                             , MainProgramClass <$> classParser <?> "class definition"]
  label "main block definition" $ nonIndented $ scoped ScopeTypeMain $ indentBlock $ mainBlock mainProgramDefinitions
  where
    mainBlock mainProgramDefinitions = do
      mainSymbol *> colonSymbol
      indentSome (return . MainProgram mainProgramDefinitions) statement

programParser :: Parser MainProgram
programParser = between space eof mainParser

parseProgram :: String -> Text -> Either String MainProgram
parseProgram filename input = first errorBundlePretty $ runParser (evalStateT programParser def) filename input

newIdentifierCheck :: (Text ->ParserState -> ParserState) -> Parser Text
newIdentifierCheck f = do
  ident <- identifier
  exists <- existsIdentifier ident
  if exists
    then fail ("Identifier " ++ T.unpack ident ++ " already defined")
  else
    do
      modify (f ident)
      return ident

newIdentifier :: Parser Text
newIdentifier = newIdentifierCheck addIdentifier

newFunctionIdentifier :: Parser Text
newFunctionIdentifier = newIdentifierCheck addPlaceHolderFunction

indentBlock :: Parser (IndentOpt a b) -> Parser a
indentBlock = L.indentBlock scn

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentation :: Maybe Pos
indentation = Nothing

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
  argumentName <- newIdentifier
  colonSymbol
  argumentType <- simpleType
  return FunctionArgument{..}

functionParser :: Parser Function
functionParser = scoped (ScopeTypeFunction "") $ indentBlock functionBlock
  where
    functionBlock = do
      defSymbol
      functionName <- newFunctionIdentifier
      modify $ modifyScope (\(Scope _ ids vars arrs) -> Scope (ScopeTypeFunction functionName) ids vars arrs)
      functionArguments <- parens $ sepBy functionArgument commaSymbol
      arrowSymbol
      functionReturnType <- returnType
      colonSymbol
      modify $ insertFunction functionName (FunctionDefinition functionArguments functionReturnType)
      indentSome (return . Function functionName functionArguments functionReturnType) statement

whileParser :: Parser WhileLoop
whileParser = scoped ScopeTypeWhile $ indentBlock whileBlock
  where
    whileBlock = do
      whileSymbol
      whileCondition <- expr
      if expressionType whileCondition == Simple BoolType
        then colonSymbol *> indentSome (return . WhileLoop whileCondition) statement
        else fail "Only boolean expressions can be used in while condition"

ifParser :: Parser Conditional
ifParser = do
  ifBlock <- scoped ScopeTypeConditional $ indentBlock $ indentedCondition ifSymbol
  elifBlocks <- many $ scoped ScopeTypeConditional $ indentBlock $ indentedCondition elifSymbol
  elseBlock <- optional $ scoped ScopeTypeConditional $ indentBlock indentedElse
  return Conditional{..}
    where
      indentedCondition firstSymbol = do
        _ <- firstSymbol
        conditionalExpr <- expr
        if expressionType conditionalExpr == Simple BoolType
          then colonSymbol *> indentSome (return . ConditionalBlock conditionalExpr) statement
          else fail "Only boolean expressions can be used for conditions"
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

functionCallParser :: Parser FunctionCall
functionCallParser = do
  functionCallName <- identifier
  functionCallArguments <- parens $ sepBy expr commaSymbol
  return FunctionCall{..}

methodCallParser :: Parser MethodCall
methodCallParser = do
  methodCallObjectName <- selfSymbol <|> identifier
  dotSymbol
  methodCallMethodName <- identifier
  methodCallArguments <- parens $ sepBy expr commaSymbol
  return MethodCall{..}

returnParser :: Parser Statement
returnParser = do
  returnSymbol
  mExpr <- optional expr
  return (ReturnStatement mExpr)

declaration :: Parser Declaration
declaration = letSymbol *> do
  identifiers <- sepBy1 newIdentifier commaSymbol
  idType <- colonSymbol *> composedType
  rExpr <- optional $ equalSymbol *> expr
  if maybe True ((== idType) . expressionType) rExpr
    then return ()
    else fail "Expression must match type"
  case idType of
    Simple _ -> forM_  identifiers (modify . insertVariable (createVariable idType rExpr))
    ArrayType sType sz -> forM_ identifiers (modify . insertArray (createArray sType sz rExpr))
    -- TODO: Manage objects
    ClassType _ -> return ()
  return (Declaration identifiers idType rExpr)

statement :: Parser Statement
statement = choice [ continueParser
                   , breakParser
                   , passSymbol
                   , returnParser <?> "function return"
                   , ObjectAssignmentStatement <$> try objectAssignment <?> "object assignment"
                   , ArrayAssignmentStatement <$> try arrayAssignmet <?> "array assignment"
                   , SimpleAssignmentStatement <$> try simpleAssignment <?> "simple assignment"
                   , MethodCallStatement <$> try methodCallParser <?> "method call"
                   , FunctionCallStatement <$> try functionCallParser <?> "function call"
                   , WhileStatement <$> whileParser <?> "while block"
                   , ForLoopStatement <$> forParser <?> "for block"
                   , ConditionalStatement <$> ifParser <?> "conditional block"
                   , printParser <?> "print statement"
                   , DeclarationStatement <$> declaration <?> "local declaration"
                   , CreateObjectStatement <$> createObjectParser <?> "object creation"
                   , readParser]

breakParser :: Parser Statement
breakParser = do
    break <- breakSymbol
    existsFor <- existsScope ScopeTypeFor
    existsWhile <- existsScope ScopeTypeWhile
    if existsFor || existsWhile
      then return break
      else fail "Break must be inside a while or for loop."

continueParser :: Parser Statement
continueParser = do
  continue <- continueSymbol
  existsFor <- existsScope ScopeTypeFor
  existsWhile <- existsScope ScopeTypeWhile
  if existsFor || existsWhile
    then return continue
    else fail "Continue must be inside a while or for loop."

exprId :: Parser SimpleExpr
exprId = Var <$> identifier

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
  if expressionType index == Simple IntType
    then return (ArrayAccess ident index)
    else fail "Array access must be an integral expression"

exprInt :: Parser SimpleExpr
exprInt = IntLiteral <$> intLiteral

exprFloat :: Parser SimpleExpr
exprFloat = FloatLiteral <$> floatLiteral

exprBool :: Parser SimpleExpr
exprBool = BoolLiteral <$> (trueSymbol <|> falseSymbol)

exprFunctionCall :: Parser SimpleExpr
exprFunctionCall = FunctionCallExpr <$> functionCallParser

exprMethodCall :: Parser SimpleExpr
exprMethodCall = MethodCallExpr <$> methodCallParser

exprString :: Parser SimpleExpr
exprString = StringLiteral <$> stringLiteral

factor :: Parser SimpleExpr
factor = choice [ parens simpleExpr
                , try exprFloat
                , exprInt
                , exprBool
                , try exprMethodCall
                , try exprFunctionCall
                , try exprMemberAccess
                , try exprArrayAccess
                , exprString
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
  variable <- findVariable i
  equalSymbol
  e <- expr
  if expressionType e == variableType variable
    then return ()
    else fail "The types of the expression and assignment doesn't match."
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
forParser = scoped ScopeTypeFor $ indentBlock forBlock
  where
    forBlock = do
      forSymbol
      forDeclaration <- sepBy1 identifier commaSymbol
      colonSymbol
      forDeclarationType <- simpleType
      equalSymbol
      forDeclarationExpr <- expr
      if expressionType forDeclarationExpr == Simple forDeclarationType
        then return ()
        else fail "Expression type must be the same as the declaration type"
      colonSymbol
      forCondition <- expr
      if expressionType forCondition == Simple BoolType
        then return ()
        else fail "Only boolean expressions can be used in for condition"
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
classConstructorParser = scoped ScopeTypeClass $ indentBlock indentedConstructor
  where
    indentedConstructor = do
      _ <- identifier
      classConstructorParameters <- parens $ sepBy classConstructorParameter commaSymbol
      colonSymbol
      indentSome (listToConstructor $ ClassConstructor classConstructorParameters) helper
    superConstructor = do
      superSymbol
      parens $ sepBy identifier commaSymbol
    constructorAssignments = choice [ ConstructorPass <$ passSymbol
                                    , ConstructorSimpleAssignment <$> try simpleAssignment
                                    , ConstructorObjectAssignment <$> try objectAssignment
                                    , ConstructorArrayAssignment <$> try arrayAssignmet]
    helper = ConstructorSuper <$> superConstructor <|> ConstructorAssignment <$> constructorAssignments
    listToConstructor f (ConstructorSuper x N.:| xs) = f (Just x) <$> traverse checkAssignment xs
    listToConstructor f xs = f Nothing <$> traverse checkAssignment (N.toList xs)
    checkAssignment (ConstructorAssignment x) = return x
    checkAssignment _                         = fail "Expected assignment"

classInitializationParser :: Parser ClassInitialization
classInitializationParser = indentBlock initBlock
  where
    initBlock = do
      initSymbol *> colonSymbol
      indentSome listToInit helper
    helper = ClassMemberHelper <$> classMember <|> ClassConstructorHelper <$> classConstructorParser
    listToInit l = ClassInitialization <$> traverse checkMember (N.init l) <*> (checkConstructor . N.last) l
    checkMember (ClassMemberHelper x) = return x
    checkMember _ = fail "Can't have members after constructor definition"
    checkConstructor (ClassConstructorHelper x) = return x
    checkConstructor _ = fail "Constructor is required"

classParser :: Parser Class
classParser = indentBlock classBlock
  where
    classBlock = do
      classSymbol
      className <- identifier
      classFather <- optional $ parens identifier
      colonSymbol
      indentSome (listToClass $ Class className classFather) helper
    helper = ClassHelperInit <$> classInitializationParser <|> ClassHelperMethod <$> functionParser
    listToClass f (ClassHelperInit x N.:| xs) = f x <$> traverse checkMember xs
    listToClass _ _ = fail "Initialization block is required"
    checkMember (ClassHelperMethod f) = return f
    checkMember _ = fail "Only one initialization block is allowed"

createObjectParser :: Parser CreateObject
createObjectParser =  do
  createSymbol
  variableName <- identifier
  colonSymbol
  objectName <- identifier
  identifiers <- parens $ sepBy identifier commaSymbol
  return (CreateObject variableName objectName identifiers)
