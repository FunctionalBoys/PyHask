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
import qualified Data.List.NonEmpty                 as N
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Expressions
import           GenUtils
import           Lexer
import           ParserTypes
import           Text.Megaparsec                    hiding (sepBy1, some)
import           Text.Megaparsec.Char
import           Utils

mainParser :: Parser MainProgram
mainParser = do
  mainProgramDefinitions <- many $ nonIndented $ choice [ MainProgramFunction <$> functionParser <?> "function definition"
                                                             , MainProgramDeclaration <$> declaration <?> "global variable"
                                                             , MainProgramClass <$> classParser <?> "class definition"]
  label "main block definition" $ nonIndented $ scoped ScopeTypeMain $ (indentBlock $ mainBlock mainProgramDefinitions) <* registerQuadruple QuadEnd
  where
    mainBlock mainProgramDefinitions = do
      mainSymbol *> colonSymbol
      indentSome (return . MainProgram mainProgramDefinitions) statement

programParser :: Parser MainProgram
programParser = between space eof mainParser

parseProgram :: String -> Text -> Either String (MainProgram, ParserState)
parseProgram filename input = first errorBundlePretty $ runParser (runStateT programParser def) filename input

newIdentifierCheck :: (Text -> ParserState -> ParserState) -> Parser Text
newIdentifierCheck f = do
  ident <- identifier
  exists <- existsIdentifier ident
  guardFail (not exists) ("Identifier " ++ T.unpack ident ++ " already defined")
  ident <$ modify (f ident)

newIdentifier :: Parser Text
newIdentifier = newIdentifierCheck addIdentifier

simpleType :: Parser SimpleType
simpleType = choice [intSymbol, boolSymbol, floatSymbol, charSymbol]

composedType :: Parser ComposedType
composedType = try (ArrayType <$> simpleType <*> some arrayDim) <|> Simple <$> simpleType <|> ClassType <$> identifier
  where
    arrayDim = do
      dim <- brackets intLiteral
      guardFail (dim > 0) "Array dimension cannot be smaller than 1"
      return dim

returnType :: Parser ReturnType
returnType = voidSymbol <|> ValueReturn <$> simpleType

functionArgument :: Parser FunctionArgument
functionArgument = do
  argumentName <- newIdentifier
  colonSymbol
  argumentType <- simpleType
  address <- nextVarAddress argumentType
  modify $ insertVariable (Variable (Simple argumentType) True address) argumentName
  return FunctionArgument{..}

functionParser :: Parser Function
functionParser = scoped ScopePlaceholder $ (indentBlock functionBlock) <* registerQuadruple QuadEndFunc
  where
    functionBlock = do
      defSymbol
      functionName <- newIdentifier
      functionDefinitionArguments <- parens $ sepBy functionArgument commaSymbol
      arrowSymbol
      functionDefinitionReturnType <- returnType
      case functionDefinitionReturnType of
        ValueReturn sType -> do
          address <- nextGlobalVarAddress sType
          modify $ insertGlobalVariable (Variable (Simple sType) True address) functionName
        _ -> return ()
      colonSymbol
      functionDefinitionVarMB <- gets currentMemoryBlock
      functionDefinitionTempMB <- gets currentTempBlock
      functionDefinitionIP <- gets quadruplesCounter
      let fDefinition = FunctionDefinition{..}
      modify $ modifyScope (\(Scope _ ids vars mVars mTemp) -> Scope (ScopeTypeFunction functionName) ids vars mVars mTemp)
      maybeClass <- maybeInsideClass
      maybe (modify $ insertFunction functionName fDefinition) (f fDefinition functionName) maybeClass
      indentSome (return . Function functionName functionDefinitionArguments functionDefinitionReturnType) statement
    f fDefinition fName clsName = modify $ insertMethodToClass clsName fName fDefinition

whileParser :: Parser WhileLoop
whileParser = do
  cont <- gets quadruplesCounter
  whileLoop@WhileLoop{whileConditionEnd = end} <- scoped ScopeTypeWhile $ indentBlock whileBlock
  registerQuadruple $ QuadGOTO cont
  cont2 <- gets quadruplesCounter
  safeQuadrupleUpdate (fillGOTOF cont2) end
  return whileLoop
  where
    whileBlock = do
      whileSymbol
      whileCondition@Expr{memoryAddress = address} <- expr
      guardFail (expressionType whileCondition == Simple BoolType) "Only boolean expressions allowed as conditions"
      whileConditionEnd <- gets quadruplesCounter
      registerQuadruple $ QuadFPlaceholder address
      colonSymbol *> indentSome (return . WhileLoop whileCondition whileConditionEnd) statement

ifParser :: Parser Conditional
ifParser = do
  ifBlock <- scoped ScopeTypeConditional $ (indentBlock $ indentedCondition ifSymbol) <* registerQuadruple QuadGOTOPlaceholder
  elifBlocks <- many $ scoped ScopeTypeConditional $ (indentBlock $ indentedCondition elifSymbol) <* registerQuadruple QuadGOTOPlaceholder
  let ends = conditionalEnd <$> ifBlock : elifBlocks
  elseStart <- gets quadruplesCounter
  let starts = (conditionalStart <$> elifBlocks) ++ [elseStart]
  let zipped = zip ends starts
  forM_ zipped updateEnds
  elseBlock <- optional $ scoped ScopeTypeConditional $ indentBlock indentedElse
  end <- gets quadruplesCounter
  forM_ (subtract 1 <$> starts) (updateInconditionals end)
  return Conditional{..}
    where
      indentedCondition firstSymbol = do
        _ <- firstSymbol
        conditionalStart <- gets quadruplesCounter
        conditionalExpr@Expr{memoryAddress = address} <- expr
        conditionalEnd <- gets quadruplesCounter
        registerQuadruple $ QuadFPlaceholder address
        guardFail (expressionType conditionalExpr == Simple BoolType) "Only boolean expressions can be used for conditions"
        colonSymbol *> indentSome (return . ConditionalBlock conditionalStart conditionalExpr conditionalEnd) statement
      indentedElse = elseSymbol *> indentSome return statement
      updateEnds (end, start) = safeQuadrupleUpdate (fillGOTOF start) end
      updateInconditionals end = safeQuadrupleUpdate (fillGOTO end)

printParser :: Parser Statement
printParser = do
  printSymbol
  pExpr@(Expr _ _ address) <- parens $ expr >>= exprSimpleType
  registerQuadruple $ QuadPrint address
  return (PrintStatement pExpr)

readParser :: Parser Statement
readParser = do
  ident <- identifier
  equalSymbol
  readSymbol
  (Variable variableT _ address) <- findVariable ident
  sType <- extractSimpleType variableT
  modify $ setVariableAsInitialized ident
  registerQuadruple $ QuadRead address
  return (ReadStatement ident sType)

functionCallParser :: Parser FunctionCall
functionCallParser = do
  functionCallName <- identifier
  registerQuadruple $ QuadEra functionCallName
  fArgumentsType <- fmap (Simple . argumentType) . functionDefinitionArguments <$> findFunction functionCallName
  functionCallArguments <- parens $ sepBy expr commaSymbol
  writeParams functionCallArguments
  guardFail (fArgumentsType == fmap expressionType functionCallArguments) "Argument types do not match for function call"
  registerQuadruple $ QuadGOSUB functionCallName
  return FunctionCall{..}

methodCallParser :: Parser MethodCall
methodCallParser = do
  methodCallObjectName <- selfSymbol <|> identifier
  (Variable vType _ _) <- findVariable methodCallObjectName
  clsName <- extractClassName vType
  cls <- findClass clsName
  dotSymbol
  methodCallMethodName <- identifier
  mDefinition <- maybeFail ("No method named " ++ T.unpack methodCallMethodName) (M.lookup methodCallMethodName (classDefinitionMethods cls))
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
  guardFail (rExpr == rType) "Return type does not match function type"
  maybe (return ()) (\(Expr _ _ address) -> registerQuadruple $ QuadReturn address) mExpr
  registerQuadruple QuadEndFunc
  return (ReturnStatement mExpr)
  where
    check (Expr _ (Simple sType) _) = Just (ValueReturn sType)
    check _                         = Nothing

declaration :: Parser Declaration
declaration = letSymbol *> do
  identifiers <- sepBy1 newIdentifier commaSymbol
  idType <- colonSymbol *> composedType <?> "Variable type"
  rExpr <- optional $ equalSymbol *> expr
  guardFail (maybe True ((== idType) . expressionType) rExpr) "Expression must match type"
  case idType of
    ClassType _ -> fail "Use create statement for object declaration"
    Simple sType -> forM_  identifiers (addId sType rExpr)
    _ -> forM_  identifiers (modify . insertVariable (createVariable idType rExpr (Address (-1)))) -- TODO: TODO :TODO :TODO
  return (Declaration identifiers idType rExpr)
    where
      addId sType mExpr ident = do
        address <- nextVarAddress sType
        modify $ insertVariable (createVariable (Simple sType) mExpr address) ident
        maybe (return ()) (assign address) mExpr
      assign varAddress (Expr _ _ address) = registerQuadruple $ QuadAssign address varAddress

statement :: Parser Statement
statement = choice [ continueParser
                   , breakParser
                   , passSymbol <* registerQuadruple QuadNoOP
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
  indices <- some $ brackets expr
  let expressionTypes = expressionType <$> indices
  guardFail (all (== Simple IntType) expressionTypes) "Array access must be an integral expression"
  return (ArrayAccess ident indices)

exprInt :: Parser SimpleExpr
exprInt = do
  integer <- intLiteral
  address <- getLiteralAddress $ LiteralInt integer
  return (IntLiteral integer address)

exprFloat :: Parser SimpleExpr
exprFloat = do
  float <- floatLiteral
  address <- getLiteralAddress $ LiteralFloat float
  return (FloatLiteral float address)

exprBool :: Parser SimpleExpr
exprBool = do
  bool <- trueSymbol <|> falseSymbol
  address <- getLiteralAddress $ LiteralBool bool
  return (BoolLiteral bool address)

exprFunctionCall :: Parser SimpleExpr
exprFunctionCall = FunctionCallExpr <$> functionCallParser

exprMethodCall :: Parser SimpleExpr
exprMethodCall = MethodCallExpr <$> methodCallParser

exprString :: Parser SimpleExpr
exprString = StringLiteral <$> stringLiteral

exprChar :: Parser SimpleExpr
exprChar = do
  chr <- charLiteral
  address <- getLiteralAddress $ LiteralChar chr
  return (CharLiteral chr address)

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
                , exprChar
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
  variable@Variable{variableAddress=vAddress} <- findVariable i
  equalSymbol
  e@Expr{memoryAddress=eAddress} <- expr
  guardFail (expressionType e == variableType variable) "The types of the expression and assignment doesn't match."
  modify $ setVariableAsInitialized i
  registerQuadruple $ QuadAssign eAddress vAddress
  return (SimpleAssignment i e)

arrayAssignmet :: Parser ArrayAssignment
arrayAssignmet = do
  i <- identifier
  (aType, _) <- getArrayInfo i
  indices <- some $ brackets expr
  let indicesType = expressionType <$> indices
  guardFail (all (== Simple IntType) indicesType) "Indices must be of type int"
  equalSymbol
  e <- expr
  guardFail (expressionType e == Simple aType) "Expression must match array type"
  return (ArrayAssignment i indices e)

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
forParser = do
  forLoop@ForLoop{forConditionEnd=conditionEnd,forAssignmentStart=assignmentStart} <- scoped ScopeTypeFor $ indentBlock forBlock
  registerQuadruple $ QuadGOTO assignmentStart
  endFor <- gets quadruplesCounter
  safeQuadrupleUpdate (fillGOTOF endFor) conditionEnd
  return forLoop
  where
    forBlock = do
      forSymbol
      forDeclaration <- sepBy1 newIdentifier commaSymbol
      colonSymbol
      forDeclarationType <- simpleType
      equalSymbol
      forDeclarationExpr@(Expr _ _ address) <- expr
      forM_ forDeclaration (f forDeclarationType address)
      guardFail (expressionType forDeclarationExpr == Simple forDeclarationType) "Expression type must be the same as the declaration type"
      colonSymbol
      forConditionStart <- gets quadruplesCounter
      forCondition@Expr{memoryAddress=conditionAddress} <- expr
      guardFail (expressionType forCondition == Simple BoolType) "Only boolean expressions can be used in for condition"
      forConditionEnd <- gets quadruplesCounter
      registerQuadruple $ QuadFPlaceholder conditionAddress
      inconditionalJump <- gets quadruplesCounter
      registerQuadruple QuadGOTOPlaceholder
      colonSymbol
      forAssignmentStart <- gets quadruplesCounter
      forAssigment <- sepBy1 simpleAssignment commaSymbol
      registerQuadruple $ QuadGOTO forConditionStart
      colonSymbol
      blockStart <- gets quadruplesCounter
      safeQuadrupleUpdate (fillGOTO blockStart) inconditionalJump
      indentSome (return . ForLoop forDeclaration forDeclarationType forDeclarationExpr forCondition forConditionEnd forAssignmentStart forAssigment) statement
    f sType tAddress ident = do
      address <- nextVarAddress sType
      registerQuadruple $ QuadAssign tAddress address
      modify $ insertVariable (Variable (Simple sType) True address) ident

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
  -- TODO: Deal with this when objects are more of a thing
  modify $ insertVariable (Variable classConstructorParameterType True (Address (-1))) classConstructorParameterId
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
      -- TODO: Deal with this when objects are more a thing
      modify $ insertVariable (Variable (ClassType className) True (Address (-1))) "self"
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
  guardFail (exprsTypes == constructorTypes) "Expressions for constructor do not match"
  -- TODO: Deal with this when objects are more a thing
  modify (insertVariable (Variable (ClassType clsName) True (Address (-1))) variableName)
  registerObjectMembers cls variableName
  return $ CreateObject variableName clsName exprs
