module Parser.Expressions (exprCheck) where

import qualified Data.List.NonEmpty   as N
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Parser.AnalysisUtils
import           Parser.GenUtils
import           Parser.ParserTypes
import           Parser.Utils

exprCheck :: SimpleExpr -> Parser Expr
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
exprCheck (FunctionCallExpr (FunctionCall fName fArguments)) = do
  fDefinition <- findFunction fName
  returnType <- getValueReturn $ functionDefinitionReturnType fDefinition
  sType <- getValueReturnSimple $ functionDefinitionReturnType fDefinition
  (Variable _ _ address) <- findVariable fName
  tempAddress <- nextTempAddress sType
  registerQuadruple $ QuadAssign address tempAddress
  return (Expr (FunctionCallExpr (FunctionCall fName fArguments)) returnType tempAddress)
exprCheck (FloatConversion sExpr) = do
  (Expr _ eType eAddress) <- exprCheck sExpr
  guardFail (eType == Simple IntType) "Only int types can be converted to float"
  quadFloatConvert sExpr eAddress
-- TODO: Check this when objects are more a thing
exprCheck (MemberAccess obj member) = do
  (Variable vType _ address) <- findVariable (memberKey obj member)
  return (Expr (MemberAccess obj member) vType address)
-- TODO: Check this when functions are objects are more a thing
exprCheck (MethodCallExpr (MethodCall objName methodName arguments)) = do
  var <- findVariable objName
  clsName <- extractClassName (variableType var)
  cls <- findClass clsName
  fDefinition <- maybeFail "Method definition not found" $  M.lookup methodName (classDefinitionMethods cls)
  let returnType = functionDefinitionReturnType fDefinition
  exprType <- getValueReturn returnType
  return (Expr (MethodCallExpr (MethodCall objName methodName arguments)) exprType (Address (-1)))
exprCheck (Operate op sExpr1 sExpr2) = do
  expr1 <- exprCheck sExpr1
  expr2 <- exprCheck sExpr2
  combineExpressions op expr1 expr2

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
  | isArithmeticOperator op && t1 == t2 && isNumericType t1 = allocateAndGenerateOp op expr1 expr2 t1
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
