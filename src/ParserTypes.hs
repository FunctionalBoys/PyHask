module ParserTypes where

import           Control.Monad.State.Lazy
import           Data.Default.Class
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as N
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = StateT ParserState (Parsec Void Text)

type IndentOpt = L.IndentOpt Parser

data SimpleType = IntType | FloatType | BoolType | StringType | CharType deriving (Eq,Show)

data ComposedType = Simple SimpleType | ArrayType SimpleType Int | ClassType Text deriving (Eq,Show)

data ReturnType = ValueReturn SimpleType | VoidReturn deriving (Eq,Show)

data Variable = Variable {  variableType :: ComposedType,
                            variableInit :: Bool
                         } deriving (Eq,Show)

data Array = Array { arrayType :: SimpleType,
                     arraySize :: Int,
                     arrayInit :: Bool
                   } deriving (Eq,Show)

data ScopeType =
    ScopeTypeFor
  | ScopeTypeConditional
  | ScopeTypeWhile
  | ScopeTypeFunction Text
  | ScopeTypeMain
  | ScopeTypeClass Text
  | ScopeTypeGlobal deriving (Eq,Show)

data Scope = Scope { scopeType      :: ScopeType,
                     scopeIdentifiers :: [Text],
                     scopeVariables :: M.Map Text Variable,
                     scopeArrays    :: M.Map Text Array
                   } deriving (Eq,Show)

data FunctionDefinition = FunctionDefinition { functionDefinitionArguments  :: [FunctionArgument],
                                               functionDefinitionReturnType :: ReturnType
                                            } deriving (Eq,Show)


data ParserState = ParserState { scopes               :: NonEmpty Scope,
                                 functionDefinitions  :: M.Map Text FunctionDefinition,
                                 classDefinitions     :: M.Map Text ClassDefinition
                               } deriving (Eq,Show)

instance Default ParserState where
  def = ParserState (Scope ScopeTypeGlobal [] M.empty M.empty N.:| []) M.empty M.empty

data SimpleAssignment = SimpleAssignment { assignmentName :: Text,
                                           assignmentExpr :: Expr
                                         } deriving (Eq,Show)

data ArrayAssignment = ArrayAssignment { arrayAssigmnentName  :: Text,
                                         arrayAssignmentIndex :: Expr,
                                         arrayAssignmentExpr  :: Expr
                                       } deriving (Eq,Show)

data ObjectAssignment = ObjectAssignment { objectAssignmentName   :: Text,
                                           objectAssignmentMember :: Text,
                                           objectAssignmentExpr   :: Expr
                                         } deriving (Eq,Show)

data MainProgramDefinition =
    MainProgramClass Class
  | MainProgramFunction Function
  | MainProgramDeclaration Declaration deriving (Eq,Show)

data MainProgram = MainProgram { mainProgramDefinitions :: [MainProgramDefinition],
                                 mainStatements :: NonEmpty Statement
                               } deriving (Eq,Show)

data Statement =
    Continue
  | Break
  | Pass
  | ForLoopStatement ForLoop
  | SimpleAssignmentStatement SimpleAssignment
  | ArrayAssignmentStatement ArrayAssignment
  | ObjectAssignmentStatement ObjectAssignment
  | ConditionalStatement Conditional
  | WhileStatement WhileLoop
  | PrintStatement Expr
  | ReadStatement Text SimpleType
  | CreateObjectStatement CreateObject
  | ReturnStatement (Maybe Expr)
  | FunctionCallStatement FunctionCall
  | MethodCallStatement MethodCall
  | DeclarationStatement Declaration deriving (Eq,Show)

data Declaration = Declaration { declarationIdentifiers :: NonEmpty Text,
                                 declarationType        :: ComposedType,
                                 declarationInit        :: Maybe Expr
                               } deriving (Eq,Show)

data FunctionArgument = FunctionArgument { argumentName :: Text,
                                           argumentType :: SimpleType
                                         } deriving (Eq,Show)

data Function = Function { functionName       :: Text,
                           functionArguments  :: [FunctionArgument],
                           functionReturnType :: ReturnType,
                           functionStatements :: NonEmpty Statement
                         } deriving (Eq,Show)

data FunctionCall = FunctionCall { functionCallName      :: Text,
                                   functionCallArguments :: [Expr]
                                 } deriving (Eq,Show)

data MethodCall = MethodCall { methodCallObjectName :: Text,
                               methodCallMethodName :: Text,
                               methodCallArguments  :: [Expr]
                             } deriving (Eq,Show)

data ClassMember = ClassMember { memberIdentifier :: Text,
                                 memberType       :: ComposedType
                               } deriving (Eq,Show)

data ClassConstructorAssignment =
    ConstructorSimpleAssignment SimpleAssignment
  | ConstructorArrayAssignment ArrayAssignment
  | ConstructorObjectAssignment ObjectAssignment
  | ConstructorPass deriving (Eq,Show)

data ClassConstructorParameter = ClassConstructorParameter { classConstructorParameterId :: Text,
                                                             classConstructorParemeterType :: ComposedType
                                                           } deriving (Eq,Show)

data ClassConstructorHelper = ConstructorSuper [Text] | ConstructorAssignment ClassConstructorAssignment

data ClassConstructor = ClassConstructor { classConstructorParameters :: [ClassConstructorParameter],
                                           classSuperConstructor :: Maybe [Text],
                                           classConstructorAssignment :: [ClassConstructorAssignment]
                                         } deriving (Eq,Show)

data ClassInitHelper = ClassConstructorHelper ClassConstructor | ClassMemberHelper ClassMember

data ClassInitialization = ClassInitialization { classMembers :: [ClassMember],
                                                 classConstructor :: ClassConstructor
                                               } deriving (Eq,Show)

data ClassHelper = ClassHelperInit ClassInitialization | ClassHelperMethod Function

data Class = Class { className           :: Text,
                     classFather         :: Maybe Text,
                     classInitialization :: ClassInitialization,
                     classMethods        :: [Function]
                   } deriving (Eq,Show)

data ClassDefinition = ClassDefinition { classDefinitionFather          :: Maybe Text,
                                         classDefinitionMembers         :: [ClassMember],
                                         classDefinitionConstructor     :: ClassConstructor,
                                         classDefinitionMethods         :: [FunctionDefinition]
                                       } deriving (Eq,Show)

data Op = Sum | Minus | Times | Div | Exp | Eq | NEq | Lt | Gt | Lte | Gte | And | Or deriving (Eq,Show)

data Expr = Expr { innerExpression :: SimpleExpr,
                   expressionType  :: ComposedType
                 } deriving (Eq,Show)

data SimpleExpr =
    Var Text
  | IntLiteral Int
  | FloatLiteral Double
  | BoolLiteral Bool
  | FunctionCallExpr FunctionCall
  | MethodCallExpr MethodCall
  | MemberAccess Text Text
  | Not SimpleExpr
  | Neg SimpleExpr
  | StringLiteral Text
  | Operate Op SimpleExpr SimpleExpr
  | ArrayAccess Text Expr deriving (Eq, Show)

data WhileLoop = WhileLoop { whileCondition  :: Expr,
                             whileStatements :: NonEmpty Statement
                           } deriving (Eq,Show)

data ForLoop = ForLoop { forDeclaration     :: NonEmpty Text,
                         forDeclarationType :: SimpleType,
                         forDeclarationExpr :: Expr,
                         forCondition       :: Expr,
                         forAssignment      :: SimpleAssignment,
                         forStatements      :: NonEmpty Statement
                        } deriving (Eq,Show)

data ConditionalBlock = ConditionalBlock { conditionalExpr :: Expr,
                                           conditionalStatements :: NonEmpty Statement
                                         } deriving (Eq,Show)

data Conditional = Conditional { ifBlock    :: ConditionalBlock,
                                 elifBlocks :: [ConditionalBlock],
                                 elseBlock  :: Maybe (NonEmpty Statement)
                               } deriving (Eq,Show)

data CreateObject = CreateObject {  variableName :: Text,
                                    objectName   :: Text,
                                    identifiers  :: [Text]
                                } deriving (Eq,Show)
