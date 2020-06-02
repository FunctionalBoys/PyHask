{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parser.Parser (parseProgram) where

import           Control.Monad.Combinators.NonEmpty
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Default.Class
import qualified Data.List.NonEmpty                 as N
import           Data.Maybe
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Parser.AnalysisUtils
import           Parser.Expressions
import           Parser.GenUtils
import           Parser.Lexer
import           Parser.ParserTypes
import           Parser.Utils
import           Text.Megaparsec                    hiding (sepBy1, some)
import           Text.Megaparsec.Char

mainParser :: Parser MainProgram
mainParser = do
  mainProgramElements <- some $ nonIndented $ choice [ MainProgramFunction <$> functionParser Nothing True <?> "function definition"
                                                             , MainProgramStatement <$> statement <?> "program statement"
                                                             , MainProgramClass <$> classParser <?> "class definition"]
  registerQuadruple QuadEnd
  return MainProgram{..}

programParser :: Parser MainProgram
programParser = between space (space *> eof) mainParser

parseProgram :: String -> Text -> Either String ParserState
parseProgram filename input = bimap errorBundlePretty snd $ runParser (runStateT programParser def) filename input

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
  argumentT <- simpleType
  let argumentType = Simple argumentT
  argumentAddress <- nextVarAddress argumentT
  modify $ insertVariable (Variable argumentType True argumentAddress) argumentName
  return FunctionArgument{..}

functionParser :: Maybe Text -> Bool -> Parser Function
functionParser givenName checkReturn = do
  ignoreFunction <- gets quadruplesCounter
  registerQuadruple QuadGOTOPlaceholder
  function <- scoped ScopePlaceholder $ indentBlock functionBlock
  registerQuadruple QuadEndFunc
  outsideFunction <- gets quadruplesCounter
  safeQuadrupleUpdate (fillGOTO outsideFunction) ignoreFunction
  guardFail (functionReturns function) "Function must have a return as its last statement"
  return function
  where
    functionBlock = do
      when checkReturn defSymbol
      functionName <- newIdentifier
      guardFail (maybe True (== functionName) givenName) "Function name does not match given name"
      arguments <- parens $ sepBy functionArgument commaSymbol
      maybeClass <- maybeInsideClass
      sArguments <- selfArgument maybeClass arguments
      functionDefinitionArguments <- superArgument maybeClass sArguments
      let trueFunctionName = maybe functionName (addPrefix functionName) maybeClass
      functionDefinitionReturnType <- if checkReturn
        then arrowSymbol *> returnType
        else return VoidReturn
      case functionDefinitionReturnType of
        ValueReturn sType -> do
          address <- nextGlobalVarAddress sType
          modify $ insertGlobalVariable (Variable (Simple sType) True address) trueFunctionName
        _ -> return ()
      colonSymbol
      functionDefinitionVarMB <- gets currentMemoryBlock
      functionDefinitionTempMB <- gets currentTempBlock
      functionDefinitionIP <- gets quadruplesCounter
      let fDefinition = FunctionDefinition{..}
      modify $ modifyScope (\(Scope _ ids vars mVars mTemp) -> Scope (ScopeTypeFunction trueFunctionName) ids vars mVars mTemp)
      modify $ insertFunction trueFunctionName fDefinition
      indentSome (return . Function functionName functionDefinitionArguments functionDefinitionReturnType) statement
    addPrefix fName clsName = clsName <> "." <> fName
    selfArgument (Just clsName) arguments = do
      argumentAddress <- nextVarAddress IntType
      let argumentName = "self"
      let argumentType = ClassType clsName
      modify $ insertVariable (Variable argumentType True argumentAddress) argumentName
      let fArgument = FunctionArgument{..}
      return $ fArgument : arguments
    selfArgument Nothing arguments = return arguments
    superArgument (Just clsName) arguments = do
      (ClassDefinition mFather _) <- findClass clsName
      case mFather of
        Nothing -> return arguments
        Just fatherClass -> do
          argumentAddress <- nextVarAddress IntType
          let argumentName = "super"
          let argumentType = ClassType fatherClass
          modify $ insertVariable (Variable argumentType True argumentAddress) argumentName
          let fArgument = FunctionArgument{..}
          return $ fArgument : arguments
    superArgument Nothing arguments = return arguments

whileParser :: Parser WhileLoop
whileParser = do
  cont <- gets quadruplesCounter
  (whileLoop@WhileLoop{whileConditionEnd = end}, scopeData) <- dataScoped (ScopeTypeWhile [] []) $ indentBlock whileBlock
  registerQuadruple $ QuadGOTO cont
  cont2 <- gets quadruplesCounter
  safeQuadrupleUpdate (fillGOTOF cont2) end
  writeLoopJumps cont cont2 scopeData
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
  fArgumentsType <- fmap argumentType . functionDefinitionArguments <$> findFunction functionCallName
  functionCallArguments <- parens $ sepBy expr commaSymbol
  writeParams functionCallArguments
  guardFail (fArgumentsType == fmap expressionType functionCallArguments) "Argument types do not match for function call"
  registerQuadruple $ QuadGOSUB functionCallName
  return FunctionCall{..}

methodCallParser :: Parser MethodCall
methodCallParser = do
  methodCallObjectName <- selfSymbol <|> superSymbol <|> identifier
  (Variable vType _ objAddress) <- findVariable methodCallObjectName
  methodCallClassName <- extractClassName vType
  dotSymbol
  methodCallMethodName <- identifier
  classFather <- classDefinitionFather <$> findClass methodCallClassName
  let methodFunctionName = methodCallClassName <> "." <> methodCallMethodName
  let objExpr = Expr NoExpr (ClassType methodCallClassName) objAddress
  registerQuadruple $ QuadEra methodFunctionName
  mDefinition <- findFunction methodFunctionName
  arguments <- parens $ sepBy expr commaSymbol
  let methodCallArguments = case classFather of
        Nothing -> objExpr : arguments
        Just fatherName -> Expr NoExpr (ClassType fatherName) objAddress : objExpr : arguments
  writeParams methodCallArguments
  let callTypes = expressionType <$> methodCallArguments
  let defTypes = argumentType <$> functionDefinitionArguments mDefinition
  guardFail (callTypes == defTypes) "Incorrect expression types for method call"
  registerQuadruple $ QuadGOSUB methodFunctionName
  return MethodCall{..}

returnParser :: Parser Statement
returnParser = do
  returnSymbol
  mExpr <- optional expr
  let rExpr = fromMaybe VoidReturn (mExpr >>= check)
  fName <- findScopeFunctionName
  fDefinition <- findFunction fName
  let rType = functionDefinitionReturnType fDefinition
  guardFail (rExpr == rType) "Return type does not match function type"
  maybe (return ()) (\(Expr _ _ address) -> do
                        (Variable _ _ fAddress) <- findVariable fName
                        registerQuadruple $ QuadAssign address fAddress) mExpr
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
    ArrayType sType sizes -> forM_  identifiers (addArray sType sizes rExpr)
  return (Declaration identifiers idType rExpr)
    where
      addId sType mExpr ident = do
        address <- nextVarAddress sType
        modify $ insertVariable (createVariable (Simple sType) mExpr address) ident
        maybe (return ()) (assign address) mExpr
      assign varAddress (Expr _ _ address) = registerQuadruple $ QuadAssign address varAddress
      addArray sType sizes mExpr ident = do
        let arrSize = product sizes
        arrAddress <- nextVarAddressGeneral arrSize sType
        modify $ insertVariable (createVariable (ArrayType sType sizes) mExpr arrAddress) ident
        maybe (return ()) (assign arrAddress) mExpr

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
breakParser = breakSymbol <* insideLoop "break" <* addBreakToLoop

continueParser :: Parser Statement
continueParser = continueSymbol <* insideLoop "continue" <* addContinueToLoop

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
exprFunctionCall = do
  fCall@(FunctionCall fName _) <- functionCallParser
  (Variable _ _ address) <- findVariable fName
  fDefinition <- findFunction fName
  sType <- getValueReturnSimple $ functionDefinitionReturnType fDefinition
  tempAddress <- nextTempAddress sType
  registerQuadruple $ QuadAssign address tempAddress
  return $ FunctionCallExpr fCall tempAddress

exprMethodCall :: Parser SimpleExpr
exprMethodCall = do
  mCall@(MethodCall _ clsName mName _) <- methodCallParser
  let functionMethodName = clsName <> "." <> mName
  (Variable _ _ address) <- findVariable functionMethodName
  fDefinition <- findFunction functionMethodName
  sType <- getValueReturnSimple $ functionDefinitionReturnType fDefinition
  tempAddress <- nextTempAddress sType
  registerQuadruple $ QuadAssign address tempAddress
  return $ MethodCallExpr mCall tempAddress

exprString :: Parser SimpleExpr
exprString = do
  literal <-  stringLiteral
  address <- getLiteralAddress $ LiteralString literal
  return (StringLiteral literal address)

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

operatorTable :: [[Operator SimpleExpr]]
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

binary :: Parser Op -> Operator SimpleExpr
binary = InfixL . fmap Operate

rightBinary :: Parser Op -> Operator SimpleExpr
rightBinary = InfixR . fmap Operate

prefix :: Parser a -> (SimpleExpr -> SimpleExpr) -> Operator SimpleExpr
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
  (aType, boundaries) <- getArrayInfo i
  indices <- some $ brackets expr
  (Variable _ _ baseAddress) <- findVariable i
  let indicesType = expressionType <$> indices
  guardFail (all (== Simple IntType) indicesType) "Indices must be of type int"
  equalSymbol
  e@Expr{memoryAddress=address} <- expr
  guardFail (expressionType e == Simple aType) "Expression must match array type"
  offSet <- writeArrayAccess indices boundaries
  registerQuadruple $ QuadArrayAssign baseAddress offSet address
  return (ArrayAssignment i indices e)

objectAssignment :: Parser ObjectAssignment
objectAssignment = do
  obj <- selfSymbol <|> identifier
  dotSymbol
  member <- identifier
  (Variable vType _ objAddress) <- findVariable obj
  memberT <- getMemberType member vType
  equalSymbol
  e@Expr{memoryAddress=address, expressionType=eType} <- expr
  guardFail (eType == Simple memberT) "Expression type doesn't match member's"
  registerQuadruple $ QuadMemberAssign member objAddress address
  return (ObjectAssignment obj member e)

forParser :: Parser ForLoop
forParser = do
  (forLoop@ForLoop{forConditionEnd=conditionEnd,forAssignmentStart=assignmentStart},scopeData) <- dataScoped (ScopeTypeFor [] [])$ indentBlock forBlock
  registerQuadruple $ QuadGOTO assignmentStart
  endFor <- gets quadruplesCounter
  safeQuadrupleUpdate (fillGOTOF endFor) conditionEnd
  writeLoopJumps assignmentStart endFor scopeData
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
  memberType <- simpleType
  addClassMember ClassMember{..}
  where
    addClassMember member = do
      className <- findScopeClassName
      clsMembers <- classDefinitionMembers <$> findClass className
      guardFail (member `notElem` clsMembers) "Repeated class member"
      modify $ insertMemberToClass className member
      return member

classInitializationParser :: Parser ClassInitialization
classInitializationParser = indentBlock initBlock
  where
    initBlock = do
      initSymbol *> colonSymbol
      indentSome listToInit helper
    helper = ClassMemberHelper <$> classMember <|> ClassConstructorHelper <$> functionParser (Just "__init__") False
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
classParser = do
  ignoreClass <- gets quadruplesCounter
  registerQuadruple QuadGOTOPlaceholder
  cls <- scoped ScopePlaceholder $ indentBlock classBlock
  outsideClass <- gets quadruplesCounter
  safeQuadrupleUpdate (fillGOTO outsideClass) ignoreClass
  return cls
  where
    classBlock = do
      classSymbol
      className <- newIdentifier
      modify $ modifyScope (\(Scope _ ids vars mVars mTemp) -> Scope (ScopeTypeClass className) ids vars mVars mTemp)
      classFather <- optional $ parens checkIdentifierClass
      fatherMembers <- case classFather of
        Nothing -> return []
        Just fatherName -> do
          fatherDefinition <- findClass fatherName
          return $ classDefinitionMembers fatherDefinition
      modify $ insertClassDefinition className (ClassDefinition classFather fatherMembers)
      colonSymbol
      indentSome (listToClass $ Class className classFather) helper
    helper = ClassHelperInit <$> classInitializationParser <|> ClassHelperMethod <$> functionParser Nothing True
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
  ClassDefinition{classDefinitionFather=clsFather} <- findClass clsName
  exprs <- parens $ sepBy expr commaSymbol
  let constructorFunctionName = clsName <> ".__init__"
  registerQuadruple $ QuadEra constructorFunctionName
  constructorTypes <- fmap argumentType . functionDefinitionArguments <$> findFunction constructorFunctionName
  pointerAddress <- nextVarAddress IntType
  registerQuadruple $ QuadNextObjectId pointerAddress
  modify (insertVariable (Variable (ClassType clsName) True pointerAddress) variableName)
  let selfExpr = Expr NoExpr (ClassType clsName) pointerAddress
  let constructorParams = case clsFather of
        Nothing -> selfExpr : exprs
        Just fatherName -> Expr NoExpr (ClassType fatherName) pointerAddress : selfExpr : exprs
  writeParams constructorParams
  let exprsTypes = expressionType <$> constructorParams
  guardFail (exprsTypes == constructorTypes) "Expressions for constructor do not match"
  registerQuadruple $ QuadGOSUB constructorFunctionName
  return $ CreateObject variableName clsName exprs
