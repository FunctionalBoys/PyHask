{-# LANGUAGE OverloadedStrings #-}

module Parser.Expressions (exprCheck, makeExprParser) where

import qualified Data.List.NonEmpty   as N
import qualified Data.Text            as T
import           Parser.AnalysisUtils
import           Parser.GenUtils
import           Parser.ParserTypes
import           Parser.Utils
import Control.Monad.Combinators

exprCheck :: SimpleExpr -> Parser Expr
exprCheck NoExpr = fail "This should not happen"
exprCheck var@(Var ident) = do
  (Variable vType initialized address) <- findVariable ident
  guardFail initialized $ "Can't use uninitailized variable " ++ T.unpack ident
  return (Expr var vType address)
exprCheck sExpr@(IntLiteral _ a) = return (Expr sExpr (Simple IntType) a)
exprCheck sExpr@(FloatLiteral _ a) = return (Expr sExpr (Simple FloatType) a)
exprCheck sExpr@(BoolLiteral _ a) = return (Expr sExpr (Simple BoolType) a)
exprCheck sExpr@(StringLiteral sLiteral address) = return (Expr sExpr (ArrayType CharType (length sLiteral N.:| [])) address)
exprCheck sExpr@(CharLiteral _ a) = return (Expr sExpr (Simple CharType) a)
exprCheck (Not sExpr) = do
  (Expr cExpr cType address) <- exprCheck sExpr
  guardFail (cType == Simple BoolType) "Only boolean expressions can be negated"
  nAddress <- nextTempAddress BoolType
  registerQuadruple $ QuadNot address nAddress
  return (Expr (Not cExpr) cType nAddress)
exprCheck (Neg sExpr) = do
  (Expr cExpr cType address) <- exprCheck sExpr
  sType <- extractSimpleType cType
  guardFail (sType == IntType || sType == FloatType) "Only numeric values have a negative"
  nAddress <- nextTempAddress sType
  registerQuadruple $ QuadNeg address nAddress
  return (Expr (Neg cExpr) cType nAddress)
exprCheck (ArrayAccess ident indices) = do
  (aType,boundaries) <- getArrayInfo ident
  (Variable _ _ baseAddress) <- findVariable ident
  tempAddress <- nextTempAddress aType
  totalOffset <- writeArrayAccess indices boundaries
  registerQuadruple $ QuadArrayAccess baseAddress totalOffset tempAddress
  return (Expr (ArrayAccess ident indices) (Simple aType) tempAddress)
exprCheck (FunctionCallExpr (FunctionCall fName fArguments) address) = do
  fDefinition <- findFunction fName
  returnType <- getValueReturn $ functionDefinitionReturnType fDefinition
  return (Expr (FunctionCallExpr (FunctionCall fName fArguments) address) returnType address)
exprCheck (FloatConversion sExpr) = do
  (Expr _ eType eAddress) <- exprCheck sExpr
  guardFail (eType == Simple IntType) "Only int types can be converted to float"
  quadFloatConvert sExpr eAddress
exprCheck (MemberAccess obj member) = do
  (Variable vType _ address) <- findVariable obj
  memberT <- getMemberType member vType
  assignAddress <- nextTempAddress memberT
  registerQuadruple $ QuadMemberAccess member address assignAddress
  return (Expr (MemberAccess obj member) (Simple memberT) assignAddress)
exprCheck (MethodCallExpr mCall@(MethodCall _ cls member _) address) = do
  let methodFunctionName = cls <> "." <> member
  fDefinition <- findFunction methodFunctionName
  returnType <- getValueReturn $ functionDefinitionReturnType fDefinition
  return (Expr (MethodCallExpr mCall address) returnType address)
exprCheck (Operate op sExpr1 sExpr2) = do
  expr1 <- exprCheck sExpr1
  expr2 <- exprCheck sExpr2
  combineExpressions op expr1 expr2

makeExprParser :: Parser a -> [[Operator a]] -> Parser a
makeExprParser = foldl checkPrecedence

checkPrecedence :: Parser a -> [Operator a] -> Parser a
checkPrecedence term operations = term' >>= \x -> choice [rightAssociatives' x, leftAssociatives' x, return x]
  where
    (rightAssociatives, leftAssociatives, prefixes) = foldr separateOperations ([], [], []) operations
    term' = option id (choice prefixes) <*> term
    rightAssociatives' x = do
      combiner <- choice rightAssociatives
      parsedTerm <- term' >>= \r -> rightAssociatives' r <|> return r
      return $ combiner x parsedTerm
    leftAssociatives' x = do
      combiner <- choice leftAssociatives
      parsedTerm <- term'
      let r = combiner x parsedTerm
      leftAssociatives' r <|> return r

type Operations a = ([Parser (a -> a -> a)], [Parser (a -> a -> a)], [Parser (a -> a)])

separateOperations :: Operator a -> Operations a -> Operations a
separateOperations (InfixR operation) (rights, lefts, prefixes) = (operation:rights, lefts, prefixes)
separateOperations (InfixL operation) (rights, lefts, prefixes) = (rights, operation:lefts, prefixes)
separateOperations (Prefix operation) (rights, lefts, prefixes) = (rights, lefts, operation:prefixes)

arithmeticOperations :: [Op]
arithmeticOperations = [Sum, Minus, Times, Div, Exp]

booleanOperations :: [Op]
booleanOperations = [And, Or]

comparisonOperators :: [Op]
comparisonOperators = [Eq, NEq, Lt, Gt, Lte, Gte]

isArithmeticOperator :: Op -> Bool
isArithmeticOperator op = op `elem` arithmeticOperations

isBooleanOperator :: Op -> Bool
isBooleanOperator op = op `elem` booleanOperations

isComparisonOperator :: Op -> Bool
isComparisonOperator op = op `elem` comparisonOperators

isNumericType :: SimpleType -> Bool
isNumericType IntType   = True
isNumericType FloatType = True
isNumericType _         = False

quadFloatConvert :: SimpleExpr -> Address -> Parser Expr
quadFloatConvert sExpr address = do
  nAddress <- nextTempAddress FloatType
  registerQuadruple $ QuadFloatConvert address nAddress
  return $ Expr (FloatConversion sExpr) (Simple FloatType) nAddress

convertToFloat :: Expr -> Parser Expr
convertToFloat expr@Expr{expressionType=Simple FloatType} = return expr
convertToFloat (Expr sExpr (Simple IntType) address) = quadFloatConvert sExpr address
convertToFloat _ = fail "Attempting to convert not integer value"

allocateAndGenerateOp :: Op -> Expr  -> Expr -> SimpleType -> Parser Expr
allocateAndGenerateOp op (Expr sExpr1 _ a1) (Expr sExpr2 _ a2) sType = do
  address <- nextTempAddress sType
  registerQuadruple $ QuadOp op a1 a2 address
  return $ Expr (Operate op sExpr1 sExpr2) (Simple sType) address

-- Sum | Minus | Times | Div | Exp | Eq | NEq | Lt | Gt | Lte | Gte | And | Or
combineExpressions :: Op -> Expr -> Expr -> Parser Expr
combineExpressions op expr1@(Expr _ (Simple t1) _) expr2@(Expr _ (Simple t2) _)
  | isArithmeticOperator op && t1 == t2 && isNumericType t1 = allocateAndGenerateOp op expr1 expr2 (if op == Exp then FloatType else t1)
  | isArithmeticOperator op && isNumericType t1 && isNumericType t2 = do
      fExpr1 <- convertToFloat expr1
      fExpr2 <- convertToFloat expr2
      allocateAndGenerateOp op fExpr1 fExpr2 FloatType
combineExpressions op expr1@(Expr _ (Simple t1) _) expr2@(Expr _ (Simple t2) _)
  | isBooleanOperator op || isComparisonOperator op && t1 == t2 = allocateAndGenerateOp op expr1 expr2 BoolType
  | isBooleanOperator op || isComparisonOperator op && isNumericType t1 && isNumericType t2 = do
      fExpr1 <- convertToFloat expr1
      fExpr2 <- convertToFloat expr2
      allocateAndGenerateOp op fExpr1 fExpr2 BoolType
combineExpressions _ _ _ = fail "No operation allowed between expressions"
