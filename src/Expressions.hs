module Expressions where

import           AnalysisUtils
import qualified Data.Map      as M
import qualified Data.Text     as T
import           ParserTypes
import           Utils

exprCheck :: SimpleExpr -> Parser Expr
exprCheck (Var ident) = do
  (Variable vType initialized address) <- findVariable ident
  if not initialized
    then fail $ "Can't use uninitailized variable " ++ T.unpack ident
  else
    return (Expr (Var ident) vType address)
exprCheck (IntLiteral integer) = return (Expr (IntLiteral integer) (Simple IntType))
exprCheck (FloatLiteral float) = return (Expr (FloatLiteral float) (Simple FloatType))
exprCheck (BoolLiteral bool) = return (Expr (BoolLiteral bool) (Simple BoolType))
exprCheck (StringLiteral sLiteral) = return (Expr (StringLiteral sLiteral) (ArrayType CharType (length sLiteral)))
exprCheck (CharLiteral cLiteral) = return (Expr (CharLiteral cLiteral) (Simple CharType))
exprCheck (Not sExpr) = do
  (Expr cExpr cType) <- exprCheck sExpr
  if cType == Simple BoolType
    then return (Expr (Not cExpr) cType)
    else fail "Only boolean expressions can be negated"
exprCheck (Neg sExpr) = do
  (Expr cExpr cType) <- exprCheck sExpr
  if cType == Simple IntType || cType == Simple FloatType
    then return (Expr (Neg cExpr) cType)
    else fail "Only numeric values have a negative"
exprCheck (ArrayAccess ident (Expr (IntLiteral integer) (Simple IntType))) = do
  (aType,sz) <- getArrayInfo ident
  if integer >= sz
    then fail "Index out of bounds"
    else return (Expr (ArrayAccess ident (Expr (IntLiteral integer) (Simple IntType))) (Simple aType))
exprCheck (ArrayAccess ident (Expr sExpr (Simple IntType))) = do
  (aType,_) <- getArrayInfo ident
  return (Expr (ArrayAccess ident (Expr sExpr (Simple IntType))) (Simple aType))
exprCheck (ArrayAccess _ _) = fail "Array index must be of integral type"
exprCheck (FunctionCallExpr (FunctionCall fName fArguments)) = do
  fDefinition <- findFunction fName
  returnType <- getValueReturn $ functionDefinitionReturnType fDefinition
  return (Expr (FunctionCallExpr (FunctionCall fName fArguments)) returnType)
exprCheck (FloatConversion sExpr) = do
  value <- exprCheck sExpr
  if expressionType value == Simple IntType
    then return (Expr (FloatConversion sExpr) (Simple FloatType))
    else fail "Only Int types can be converted to Float"
exprCheck (MemberAccess obj member) = do
  var <- findVariable (memberKey obj member)
  return (Expr (MemberAccess obj member) (variableType var))
exprCheck (MethodCallExpr (MethodCall objName methodName arguments)) = do
  var <- findVariable objName
  clsName <- extractClassName (variableType var)
  cls <- findClass clsName
  fDefinition <- maybeFail "Method definition not found" $  M.lookup methodName (classDefinitionMethods cls)
  let returnType = functionDefinitionReturnType fDefinition
  exprType <- getValueReturn returnType
  return (Expr (MethodCallExpr (MethodCall objName methodName arguments)) exprType)
exprCheck (Operate op sExpr1 sExpr2) = do
  expr1 <- exprCheck sExpr1
  expr2 <- exprCheck sExpr2
  combineExpressions op expr1 expr2

-- Sum | Minus | Times | Div | Exp | Eq | NEq | Lt | Gt | Lte | Gte | And | Or
combineExpressions :: Op -> Expr -> Expr -> Parser Expr
combineExpressions Sum (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Sum sExpr1 sExpr2) (Simple IntType))
combineExpressions Sum (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Sum sExpr1 (FloatConversion sExpr2)) (Simple FloatType))
combineExpressions Sum (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Sum (FloatConversion sExpr1) sExpr2) (Simple FloatType))
combineExpressions Sum (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Sum sExpr1 sExpr2) (Simple FloatType))
combineExpressions Minus (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Minus sExpr1 sExpr2) (Simple IntType))
combineExpressions Minus (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Minus sExpr1 (FloatConversion sExpr2)) (Simple FloatType))
combineExpressions Minus (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Minus (FloatConversion sExpr1) sExpr2) (Simple FloatType))
combineExpressions Minus (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Minus sExpr1 sExpr2) (Simple FloatType))
combineExpressions Times (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Times sExpr1 sExpr2) (Simple IntType))
combineExpressions Times (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Times sExpr1 (FloatConversion sExpr2)) (Simple FloatType))
combineExpressions Times (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Times (FloatConversion sExpr1) sExpr2) (Simple FloatType))
combineExpressions Times (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Times sExpr1 sExpr2) (Simple FloatType))
combineExpressions Div (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Div (FloatConversion sExpr1) (FloatConversion sExpr2)) (Simple FloatType))
combineExpressions Div (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Div sExpr1 (FloatConversion sExpr2)) (Simple FloatType))
combineExpressions Div (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Div (FloatConversion sExpr1) sExpr2) (Simple FloatType))
combineExpressions Div (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple FloatType)) = return  (Expr (Operate Div sExpr1 sExpr2) (Simple FloatType))
combineExpressions Exp (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Exp (FloatConversion sExpr1) (FloatConversion sExpr2)) (Simple FloatType))
combineExpressions Exp (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple IntType)) = return (Expr (Operate Exp sExpr1 (FloatConversion sExpr2)) (Simple FloatType))
combineExpressions Exp (Expr sExpr1 (Simple IntType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Exp (FloatConversion sExpr1) sExpr2) (Simple FloatType))
combineExpressions Exp (Expr sExpr1 (Simple FloatType)) (Expr sExpr2 (Simple FloatType)) = return (Expr (Operate Exp sExpr1 sExpr2) (Simple FloatType))
combineExpressions And (Expr sExpr1 (Simple BoolType)) (Expr sExpr2 (Simple BoolType)) = return (Expr (Operate And sExpr1 sExpr2) (Simple BoolType))
combineExpressions Or (Expr sExpr1 (Simple BoolType)) (Expr sExpr2 (Simple BoolType)) = return (Expr (Operate Or sExpr1 sExpr2) (Simple BoolType))
combineExpressions _ (Expr _ (ClassType _)) (Expr _ (ClassType _)) = fail "No operators available between classes."
combineExpressions op (Expr sExpr1 t1) (Expr sExpr2 t2)
  | t1 == t2 && (t1 == Simple IntType || t1 == Simple FloatType) = return (Expr (Operate op sExpr1 sExpr2) (Simple BoolType))
  | otherwise = fail "Types can not be compared."
