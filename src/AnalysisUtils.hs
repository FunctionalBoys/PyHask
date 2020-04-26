module AnalysisUtils where

import           Control.Monad.Combinators
import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as N
import qualified Data.Map.Strict           as M
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           ParserTypes


--TODO: Actually do this
exprCheck :: SimpleExpr -> Parser Expr
exprCheck (Var ident) = do
  (Variable vType initialized) <- findVariable ident
  if not initialized
    then fail $ "Can't use uninitailized variable " ++ T.unpack ident
  else
    return (Expr (Var ident) vType)
exprCheck (IntLiteral integer) = return (Expr (IntLiteral integer) (Simple IntType))
exprCheck (FloatLiteral float) = return (Expr (FloatLiteral float) (Simple FloatType))
exprCheck (BoolLiteral bool) = return (Expr (BoolLiteral bool) (Simple BoolType))
exprCheck (StringLiteral sLiteral) = return (Expr (StringLiteral sLiteral) (Simple StringType))
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
  return (Expr (FunctionCallExpr (FunctionCall fName fArguments)) (returnType))
exprCheck (FloatConversion sExpr) = do
  value <- exprCheck sExpr
  if(expressionType value == Simple IntType)
    then return (Expr (FloatConversion sExpr) (Simple FloatType))
    else fail $ "Only Int types can be converted to Float"
exprCheck (MemberAccess t1 t2) = return (Expr (MemberAccess t1 t2) (Simple IntType))
exprCheck (MethodCallExpr _) = return (Expr (IntLiteral 1) (Simple IntType))
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
  | t1 == t2 && (t1 == (Simple IntType) || t1 == (Simple FloatType)) && (t2 == (Simple IntType) || t2 == (Simple FloatType)) = return (Expr (Operate op sExpr1 sExpr2) (Simple BoolType))
  | otherwise = fail "Types can not be compared."


getValueReturn :: ReturnType -> Parser ComposedType
getValueReturn (ValueReturn sType) = return (Simple sType)
getValueReturn _ = fail "Only functions that return values can be used in expression."

exprSimpleType :: Expr -> Parser Expr
exprSimpleType (Expr sExpr (Simple sType)) = return (Expr sExpr (Simple sType))
exprSimpleType _ = fail "Expected a simple type"

existsIdentifier :: Text -> Parser Bool
existsIdentifier identifier = do
    c <- existsClass identifier
    f <- existsFunction identifier
    s <- existsInScope identifier
    return (c || f || s)

existsClass :: Text -> Parser Bool
existsClass classIdentifier = do
    definitions <- gets classDefinitions
    return $ M.member classIdentifier definitions

existsFunction :: Text -> Parser Bool
existsFunction functionIdentifier = do
    definitions <- gets functionDefinitions
    return $ M.member functionIdentifier definitions

existsInScope :: Text -> Parser Bool
existsInScope variableIdentifier = do
    list <- gets scopes
    return $ any (elem variableIdentifier) (fmap scopeIdentifiers list)

addIdentifier :: Text -> ParserState -> ParserState
addIdentifier ident = modifyScope (\(Scope st ids v) -> Scope st (ident:ids) v)

maybeToParser :: String -> Maybe a -> Parser a
maybeToParser e = maybe (fail e) return

guardFail :: Bool -> String -> Parser ()
guardFail True _ = return ()
guardFail False s = fail s

findVariable :: Text -> Parser Variable
findVariable ident = do
  maps <- fmap scopeVariables <$> gets scopes
  maybeToParser ("Variable " ++ T.unpack ident ++ " not found") $ asum $ fmap (M.lookup ident) maps

getArrayInfoFromType :: ComposedType -> Parser (SimpleType, Int)
getArrayInfoFromType (ArrayType sType sz) = return (sType, sz)
getArrayInfoFromType _ = fail "Type is not an array"

getArrayInfo :: Text -> Parser (SimpleType, Int)
getArrayInfo ident = (variableType <$> findVariable ident) >>= getArrayInfoFromType

existsScope :: ScopeType -> Parser Bool
existsScope scopeT = do
  scopeTypes <- fmap scopeType <$> gets scopes
  return (scopeT `elem` scopeTypes)

findFunction :: Text -> Parser FunctionDefinition
findFunction fName = do
  fDefinitions <- gets functionDefinitions
  maybeToParser ("Function " ++ T.unpack fName ++ " doesn't exist") (M.lookup fName fDefinitions)

findScopeFunctionName :: Parser Text
findScopeFunctionName = do
  sTypes <- fmap scopeType <$> gets scopes
  maybeToParser "No parent function" $ asum $ fmap f sTypes
  where
    f (ScopeTypeFunction fName) = Just fName
    f _                         = Nothing

maybeClassName :: ScopeType -> Maybe Text
maybeClassName (ScopeTypeClass name) = Just name
maybeClassName _                     = Nothing

findScopeClassName :: Parser Text
findScopeClassName = do
  sTypes <- gets $ fmap scopeType . scopes
  maybeToParser "Not inside class" $ asum $ fmap maybeClassName sTypes

maybeInsideClass :: Parser (Maybe Text)
maybeInsideClass = do
  sTypes <- gets $ fmap scopeType . scopes
  return $ asum $ fmap maybeClassName sTypes

modifyScopes :: (NonEmpty Scope -> NonEmpty Scope) -> ParserState -> ParserState
modifyScopes f (ParserState s d c) = ParserState (f s) d c

modifyScope :: (Scope -> Scope) -> ParserState -> ParserState
modifyScope f (ParserState (s N.:| ss) d c) = ParserState (f s N.:| ss) d c

addScope :: ScopeType -> Parser ()
addScope sType = modify (modifyScopes $ N.cons (Scope sType [] M.empty))

destroyScope :: Parser ()
destroyScope = do
  mScopes <- snd . N.uncons <$> gets scopes
  maybe (fail "Can't destroy all scopes") (\s -> modify(modifyScopes $ const s)) mScopes

scoped :: ScopeType -> Parser a -> Parser a
scoped sType = between (addScope sType) destroyScope

createVariable :: ComposedType -> Maybe Expr -> Variable
createVariable vType expr = Variable vType (isJust expr)

insertVariable :: Variable -> Text -> ParserState -> ParserState
insertVariable v ident  = modifyScope (\(Scope sType ids variables) -> Scope sType ids (M.insert ident v variables))

insertFunction :: Text -> FunctionDefinition -> ParserState -> ParserState
insertFunction ident f (ParserState s fDefinitions c) = ParserState s (M.insert ident f fDefinitions) c

insertClassDefinition :: Text -> ClassDefinition -> ParserState -> ParserState
insertClassDefinition ident cls (ParserState s fs cDefinitions) = ParserState s fs (M.insert ident cls cDefinitions)

emptyClassDefinition :: Maybe Text -> ClassDefinition
emptyClassDefinition father = ClassDefinition father [] (ClassConstructor [] Nothing []) []

findClass :: Text -> Parser ClassDefinition
findClass cName = do
  cDefinitions <- gets classDefinitions
  maybeToParser ("Class " ++ T.unpack cName ++ " doesn't exist") (M.lookup cName cDefinitions)

insertMemberToClass :: Text -> ClassMember -> ParserState -> ParserState
insertMemberToClass clsName member (ParserState s f cDefinitions) = ParserState s f (M.update updateF clsName cDefinitions)
  where
    updateF (ClassDefinition father members constructor methods) = Just (ClassDefinition father (member:members) constructor methods)

insertMethodToClass :: Text -> FunctionDefinition -> ParserState -> ParserState
insertMethodToClass clsName method (ParserState s f cDefinitions) = ParserState s f (M.update updateF clsName cDefinitions)
  where
    updateF (ClassDefinition father members constructor methods) = Just (ClassDefinition father members constructor (method:methods))

insertConstructorToClass :: Text -> ClassConstructor -> ParserState -> ParserState
insertConstructorToClass clsName constructor (ParserState s f cDefinitions) = ParserState s f (M.update updateF clsName cDefinitions)
  where
    updateF (ClassDefinition father members _ methods) = Just (ClassDefinition father members constructor methods)
