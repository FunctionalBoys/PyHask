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
  | Declaration (NonEmpty Text) ComposedType Expr deriving (Eq,Show)

data FunctionArgument = FunctionArgument { argumentName :: Text,
                                           argumentType :: SimpleType
                                         } deriving (Eq,Show)

data Function = Function { functionName       :: Text,
                           functionArguments  :: [FunctionArgument],
                           functionReturnType :: ReturnType,
                           functionStatements :: NonEmpty Statement
                         } deriving (Eq,Show)

data Expr =
    Var Text
  | I Int
  | F Double deriving (Eq, Show)

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
