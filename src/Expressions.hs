module Expressions (exprCheck) where

import           AnalysisUtils
import qualified Data.Map      as M
import qualified Data.Text     as T
import           ParserTypes
import           Utils
import GenUtils

exprCheck :: SimpleExpr -> Parser Expr
exprCheck var@(Var ident) = do
  (Variable vType initialized address) <- findVariable ident
  guardFail initialized $ "Can't use uninitailized variable " ++ T.unpack ident
  return (Expr var vType address)
exprCheck sExpr@(IntLiteral _ a) = return (Expr sExpr (Simple IntType) a)
exprCheck sExpr@(FloatLiteral _ a) = return (Expr sExpr (Simple FloatType) a)
exprCheck sExpr@(BoolLiteral _ a) = return (Expr sExpr (Simple BoolType) a)
-- TODO: Check this when arrays are an actual thing
exprCheck (StringLiteral sLiteral) = return (Expr (StringLiteral sLiteral) (ArrayType CharType (length sLiteral)) (Address (-1)))
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
exprCheck (ArrayAccess ident (Expr (IntLiteral integer a) (Simple IntType) a2)) = do
  (aType,sz) <- getArrayInfo ident
  guardFail (integer < sz) "Index out of bounds"
  -- TODO: Check this when arrays are a thing
  return (Expr (ArrayAccess ident (Expr (IntLiteral integer a) (Simple IntType) a2)) (Simple aType) (Address (-1)))
exprCheck (ArrayAccess ident (Expr sExpr (Simple IntType) address)) = do
  (aType,_) <- getArrayInfo ident
  -- TODO: Check this when arrays are a thing
  return (Expr (ArrayAccess ident (Expr sExpr (Simple IntType) address)) (Simple aType) (Address (-1)))
exprCheck (ArrayAccess _ _) = fail "Array index must be of integral type"
exprCheck (FunctionCallExpr (FunctionCall fName fArguments)) = do
  fDefinition <- findFunction fName
  returnType <- getValueReturn $ functionDefinitionReturnType fDefinition
  -- TODO: Check this when functions are an actual thing
  return (Expr (FunctionCallExpr (FunctionCall fName fArguments)) returnType (Address (-1)))
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
