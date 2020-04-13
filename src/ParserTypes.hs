module ParserTypes where

import           Data.List.NonEmpty         (NonEmpty)
import           Data.Text                  (Text)
import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type IndentOpt = L.IndentOpt Parser

data SimpleType = IntType | FloatType | BoolType | StringType | CharType deriving (Eq,Show)

data ComposedType = Simple SimpleType | ArrayType SimpleType Int | ClassType Text deriving (Eq,Show)

data ReturnType = ValueReturn SimpleType | VoidReturn deriving (Eq,Show)

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
  | DeclarationStatement Declaration deriving (Eq,Show)

data Declaration = Declaration { declarationIdentifiers :: NonEmpty Text,
                                 declarationType :: ComposedType,
                                 declarationInit :: Maybe Expr
                               } deriving (Eq,Show)

data FunctionArgument = FunctionArgument { argumentName :: Text,
                                           argumentType :: SimpleType
                                         } deriving (Eq,Show)

data Function = Function { functionName       :: Text,
                           functionArguments  :: [FunctionArgument],
                           functionReturnType :: ReturnType,
                           functionStatements :: NonEmpty Statement
                         } deriving (Eq,Show)

data ClassMember = ClassMember { memberIdentifier :: Text,
                                 memberType :: ComposedType
                               } deriving (Eq,Show)

data ClassConstructorAssignment =
    ConstructorSimpleAssignment SimpleAssignment
  | ConstructorArrayAssignment ArrayAssignment
  | ConstructorObjectAssignment ObjectAssignment
  | ConstructorPass deriving (Eq,Show)

data ClassConstructorParameter = ClassConstructorParameter { classConstructorParameterId :: Text,
                                                             classConstructorParemeterType :: ComposedType
                                                           } deriving (Eq,Show)

data ClassConstructor = ClassConstructor { classConstructorParameters :: [ClassConstructorParameter],
                                           classSuperConstructor :: Maybe [Text],
                                           classConstructorAssignment :: NonEmpty ClassConstructorAssignment

                                         } deriving (Eq,Show)

data Op = Sum | Minus | Times | Div | Exp | Eq | NEq | Lt | Gt | Lte | Gte | And | Or deriving (Eq,Show)

data Expr = Expr SimpleExpr ComposedType deriving (Eq, Show)

data SimpleExpr =
    Var Text
  | IntLiteral Int
  | FloatLiteral Double
  | BoolLiteral Bool
  | FunctionCall Text [Expr]
  | MethodCall Text Text [Expr]
  | MemberAccess Text Text
  | Not SimpleExpr
  | Neg SimpleExpr
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

data Conditional = Conditional { ifBlock :: ConditionalBlock,
                                 elifBlocks :: [ConditionalBlock],
                                 elseBlock :: Maybe (NonEmpty Statement)
                               } deriving (Eq,Show)
