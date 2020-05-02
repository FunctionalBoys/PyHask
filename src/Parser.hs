{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parser (parseProgram) where

import           AnalysisUtils
import           Control.Monad.Combinators.Expr
import           Control.Monad.Combinators.NonEmpty
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Default.Class
import           Data.List.NonEmpty                 (NonEmpty)
import qualified Data.List.NonEmpty                 as N
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Expressions
import           Lexer
import           ParserTypes
import           Text.Megaparsec                    hiding (sepBy1)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L
import           Utils

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
    else ident <$ modify (f ident)

newIdentifier :: Parser Text
newIdentifier = newIdentifierCheck addIdentifier

objectIdentifier :: Parser Text
objectIdentifier = selfSymbol <|> identifier

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
  modify $ insertVariable (Variable (Simple argumentType) True) argumentName
  return FunctionArgument{..}

functionParser :: Parser Function
functionParser = scoped ScopePlaceholder $ indentBlock functionBlock
  where
    functionBlock = do
      defSymbol
      functionName <- newIdentifier
      modify $ modifyScope (\(Scope _ ids vars mVars mTemp) -> Scope (ScopeTypeFunction functionName) ids vars mVars mTemp)
      functionArguments <- parens $ sepBy functionArgument commaSymbol
      arrowSymbol
      functionReturnType <- returnType
      colonSymbol
      let fDefinition = FunctionDefinition functionArguments functionReturnType
      maybeClass <- maybeInsideClass
      maybe (modify $ insertFunction functionName fDefinition) (f fDefinition functionName) maybeClass
      indentSome (return . Function functionName functionArguments functionReturnType) statement
    f fDefinition fName clsName = modify $ insertMethodToClass clsName fName fDefinition

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
  e <- parens $ expr >>= exprSimpleType
  return (PrintStatement e)

readParser :: Parser Statement
readParser = do
  ident <- identifier
  equalSymbol
  readSymbol
  (Variable variableT _) <- findVariable ident
  sType <- extractSimpleType variableT
  modify $ setVariableAsInitialized ident
  return (ReadStatement ident sType)

functionCallParser :: Parser FunctionCall
functionCallParser = do
  functionCallName <- identifier
  fArgumentsType <- fmap (Simple . argumentType) . functionDefinitionArguments <$> findFunction functionCallName
  functionCallArguments <- parens $ sepBy expr commaSymbol
  if fArgumentsType == fmap expressionType functionCallArguments
    then return FunctionCall{..}
    else fail "Argument types do not match"

methodCallParser :: Parser MethodCall
methodCallParser = do
  methodCallObjectName <- selfSymbol <|> identifier
  (Variable vType _) <- findVariable methodCallObjectName
  clsName <- extractClassName vType
  cls <- findClass clsName
  dotSymbol
  methodCallMethodName <- identifier
  mDefinition <- maybeToParser ("No method named " ++ T.unpack methodCallMethodName) (M.lookup methodCallMethodName (classDefinitionMethods cls))
  methodCallArguments <- parens $ sepBy expr commaSymbol
  let callTypes = expressionType <$> methodCallArguments
  let defTypes = Simple . argumentType <$> functionDefinitionArguments mDefinition
  guardFail (callTypes == defTypes) "Incorrect expression types for method call"
  return MethodCall{..}

returnParser :: Parser Statement
returnParser = do
  returnSymbol
  mExpr <- optional expr
  let rExpr = fromMaybe VoidReturn (mExpr >>= check)
  fName <- findScopeFunctionName
  rType <- functionDefinitionReturnType <$> findFunction fName
  if rExpr == rType
    then return (ReturnStatement mExpr)
    else fail "Return type does not match function type"
  where
    check (Expr _ (Simple sType)) = Just (ValueReturn sType)
    check _                       = Nothing

declaration :: Parser Declaration
declaration = letSymbol *> do
  identifiers <- sepBy1 newIdentifier commaSymbol
  idType <- colonSymbol *> composedType
  rExpr <- optional $ equalSymbol *> expr
  if maybe True ((== idType) . expressionType) rExpr
    then return ()
    else fail "Expression must match type"
  case idType of
    ClassType _ -> fail "Use create statement for object declaration"
    _ -> forM_  identifiers (modify . insertVariable (createVariable idType rExpr))
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
breakParser = breakSymbol <* insideLoop "break"

continueParser :: Parser Statement
continueParser = continueSymbol <* insideLoop "continue"

exprId :: Parser SimpleExpr
exprId = Var <$> identifier

exprMemberAccess :: Parser SimpleExpr
exprMemberAccess = MemberAccess <$> objectIdentifier <*> (dotSymbol *> identifier)

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
  guardFail (expressionType e == variableType variable) "The types of the expression and assignment doesn't match."
  modify $ setVariableAsInitialized i
  return (SimpleAssignment i e)

arrayAssignmet :: Parser ArrayAssignment
arrayAssignmet = do
  i <- identifier
  (aType, _) <- getArrayInfo i
  a <- brackets expr
  guardFail (expressionType a == Simple IntType) "Index must be of type int"
  equalSymbol
  e <- expr
  guardFail (expressionType e == Simple aType) "Expression must match array type"
  return (ArrayAssignment i a e)

objectAssignment :: Parser ObjectAssignment
objectAssignment = do
  obj <- selfSymbol <|> identifier
  dotSymbol
  member <- identifier
  memberVariable <- findVariable (memberKey obj member)
  equalSymbol
  e <- expr
  guardFail (expressionType e == variableType memberVariable) "Expression type doesn't match member's"
  modify $ setVariableAsInitialized (memberKey obj member)
  return (ObjectAssignment obj member e)

forParser :: Parser ForLoop
forParser = scoped ScopeTypeFor $ indentBlock forBlock
  where
    forBlock = do
      forSymbol
      forDeclaration <- sepBy1 newIdentifier commaSymbol
      colonSymbol
      forDeclarationType <- simpleType
      equalSymbol
      forDeclarationExpr <- expr
      forM_ forDeclaration (modify . insertVariable (createVariable (Simple forDeclarationType) (Just forDeclarationExpr)))
      guardFail (expressionType forDeclarationExpr == Simple forDeclarationType) "Expression type must be the same as the declaration type"
      colonSymbol
      forCondition <- expr
      guardFail (expressionType forCondition == Simple BoolType) "Only boolean expressions can be used in for condition"
      colonSymbol
      forAssigment <- simpleAssignment
      colonSymbol
      indentSome (return . ForLoop forDeclaration forDeclarationType forDeclarationExpr forCondition forAssigment) statement

classMember :: Parser ClassMember
classMember = do
  letSymbol
  memberIdentifier <- newIdentifier
  colonSymbol
  memberType <- composedType
  addClassMember ClassMember{..}
  where
    addClassMember member = do
      className <- findScopeClassName
      modify $ insertMemberToClass className member
      registerMember "self" member
      return member

classConstructorParameter :: Parser ClassConstructorParameter
classConstructorParameter = do
  classConstructorParameterId <- newIdentifier
  colonSymbol
  classConstructorParameterType <- composedType
  modify $ insertVariable (Variable classConstructorParameterType True) classConstructorParameterId
  return ClassConstructorParameter{..}

classConstructorParser :: Parser ClassConstructor
classConstructorParser = scoped ScopeConstructor $ indentBlock indentedConstructor >>= addClassConstructor
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
                                    , ConstructorObjectAssignment <$> objectAssignment]
    helper = ConstructorSuper <$> superConstructor <|> ConstructorAssignment <$> constructorAssignments
    -- TODO: check super is only used if there is a parent class
    listToConstructor f (ConstructorSuper x N.:| xs) = f (Just x) <$> traverse checkAssignment xs
    listToConstructor f xs = f Nothing <$> traverse checkAssignment (N.toList xs)
    checkAssignment (ConstructorAssignment x) = return x
    checkAssignment _                         = fail "Expected assignment"
    addClassConstructor constructor = do
      className <- findScopeClassName
      modify $ insertConstructorToClass className constructor
      return constructor

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

checkIdentifierClass :: Parser Text
checkIdentifierClass = do
  ident <- identifier
  _ <- findClass ident
  return ident

classParser :: Parser Class
classParser = scoped ScopePlaceholder $ indentBlock classBlock
  where
    classBlock = do
      classSymbol
      className <- newIdentifier
      modify $ modifyScope (\(Scope _ ids vars mVars mTemp) -> Scope (ScopeTypeClass className) ids vars mVars mTemp)
      classFather <- optional $ parens checkIdentifierClass
      modify $ insertClassDefinition className (emptyClassDefinition classFather)
      modify $ insertVariable (Variable (ClassType className) True) "self"
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
  variableName <- newIdentifier
  colonSymbol
  clsName <- checkIdentifierClass
  cls <- findClass clsName
  exprs <- parens $ sepBy expr commaSymbol
  let exprsTypes = expressionType <$> exprs
  let constructorTypes = classConstructorParameterType <$> (classConstructorParameters . classDefinitionConstructor) cls
  if exprsTypes == constructorTypes
    then do
      modify (insertVariable (Variable (ClassType clsName) True) variableName)
      registerObjectMembers cls variableName
      return $ CreateObject variableName clsName exprs
    else fail "Expressions for constructor do not match"
