{-# LANGUAGE DeriveGeneric #-}

module Parser.ParserTypes where

import           Control.Monad.State.Lazy
import           Data.Default.Class
import           Data.Hashable
import           Data.Hashable.Generic      (genericHashWithSalt)
import qualified Data.HashMap.Strict        as H
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as N
import qualified Data.Map.Strict            as M
import qualified Data.Sequence              as S
import           Data.Text                  (Text)
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = StateT ParserState (Parsec Void Text)

type IndentOpt = L.IndentOpt Parser

data Operator a =
      InfixL (Parser (a -> a -> a))
    | InfixR (Parser (a -> a -> a))
    | Prefix (Parser (a -> a))

data SimpleType = IntType | FloatType | BoolType | CharType deriving (Eq,Show)

data Literal =
    LiteralInt Int
  | LiteralFloat Double
  | LiteralChar Char
  | LiteralString String
  | LiteralBool Bool deriving (Eq,Generic)

instance Show Literal where
  show (LiteralInt i) = show i
  show (LiteralFloat f) = show f
  show (LiteralChar c) = [c]
  show (LiteralString s) = s
  show (LiteralBool b) = show b

instance Hashable Literal where
  hashWithSalt s x = genericHashWithSalt s x
  {-# INLINEABLE hashWithSalt #-}

data ComposedType = Simple SimpleType | ArrayType SimpleType (NonEmpty Int) | ClassType Text deriving (Eq,Show)

data ReturnType = ValueReturn SimpleType | VoidReturn deriving (Eq,Show)

data Variable = Variable {  variableType    :: ComposedType,
                            variableInit    :: Bool,
                            variableAddress :: Address
                         } deriving (Eq,Show)
data Quad =
    QuadOp Op Address Address Address
  | QuadFloatConvert Address Address
  | QuadNot Address Address
  | QuadNeg Address Address
  | QuadRead Address
  | QuadPrint Address
  | QuadAssign Address Address
  | QuadFPlaceholder Address
  | QuadF Address Int
  | QuadTPlaceholder Address
  | QuadT Address Int
  | QuadGOTOPlaceholder
  | QuadGOTO Int
  | QuadGOSUB Text
  | QuadEra Text
  | QuadFuncParam Address Int
  | QuadEndFunc
  | QuadVerify Address Address Address
  | QuadArrayAccess Address Address Address
  | QuadArrayAssign Address Address Address
  | QuadNoOP
  | QuadEnd
  deriving (Eq,Show)

data ScopeType =
    ScopeTypeFor [Int] [Int]
  | ScopeTypeConditional
  | ScopeTypeWhile [Int] [Int]
  | ScopeTypeFunction Text
  | ScopeTypeClass Text
  | ScopePlaceholder
  | ScopeConstructor
  | ScopeTypeGlobal deriving (Eq,Show)

data TypeMemoryBlock = TypeMemoryBlock { memoryLowerBound :: Int,
                                         memoryUpperBound :: Int,
                                         currentDirection :: Int
                                       } deriving (Eq,Show)

data MemoryBlock = MemoryBlock { memoryBlockInt   :: TypeMemoryBlock,
                                 memoryBlockFloat :: TypeMemoryBlock,
                                 memoryBlockChar  :: TypeMemoryBlock,
                                 memoryBlockBool  :: TypeMemoryBlock
                               } deriving (Eq,Show)

data LiteralBlock = LiteralBlock { literalMemoryBlock :: MemoryBlock,
                                   literalAddressMap  :: H.HashMap Literal Address
                                 } deriving (Eq,Show)

data Scope = Scope { scopeType            :: ScopeType,
                     scopeIdentifiers     :: [Text],
                     scopeVariables       :: M.Map Text Variable,
                     scopeVariablesMemory :: MemoryBlock,
                     scopeTempMemory      :: MemoryBlock
                   } deriving (Eq,Show)

data FunctionDefinition = FunctionDefinition { functionDefinitionArguments  :: [FunctionArgument],
                                               functionDefinitionReturnType :: ReturnType,
                                               functionDefinitionVarMB     :: MemoryBlock,
                                               functionDefinitionTempMB    :: MemoryBlock,
                                               functionDefinitionIP        :: Int
                                             } deriving (Eq,Show)


data ParserState = ParserState { scopes               :: NonEmpty Scope,
                                 globalVariablesBlock :: MemoryBlock,
                                 globalTempBlock      :: MemoryBlock,
                                 functionDefinitions  :: M.Map Text FunctionDefinition,
                                 classDefinitions     :: M.Map Text ClassDefinition,
                                 quadruplesSequence   :: S.Seq Quad,
                                 literalBlock         :: LiteralBlock
                               } deriving (Eq,Show)

newTypeMemoryBlock :: Int -> Int -> TypeMemoryBlock
newTypeMemoryBlock i j = TypeMemoryBlock i j i

globalVariables :: MemoryBlock
globalVariables = MemoryBlock (newTypeMemoryBlock 1 1000) (newTypeMemoryBlock 1001 2000) (newTypeMemoryBlock 2001 3000) (newTypeMemoryBlock 3001 4000)

globalTemp :: MemoryBlock
globalTemp = MemoryBlock (newTypeMemoryBlock 4001 6000) (newTypeMemoryBlock 6001 8000) (newTypeMemoryBlock 8001 10000) (newTypeMemoryBlock 10001 12000)

newLocalVariables :: MemoryBlock
newLocalVariables = MemoryBlock (newTypeMemoryBlock 12001 16000) (newTypeMemoryBlock 16001 20000) (newTypeMemoryBlock 20001 24000) (newTypeMemoryBlock 24001 28000)

newLocalTemp :: MemoryBlock
newLocalTemp = MemoryBlock (newTypeMemoryBlock 28001 32000) (newTypeMemoryBlock 32001 36000) (newTypeMemoryBlock 36001 40000) (newTypeMemoryBlock 40001 44000)

literalsBlock :: MemoryBlock
literalsBlock = MemoryBlock (newTypeMemoryBlock 44001 46000) (newTypeMemoryBlock 46001 48000) (newTypeMemoryBlock 48001 50000) (newTypeMemoryBlock 50001 50002)

instance Default ParserState where
  def = ParserState (Scope ScopeTypeGlobal [] M.empty globalVariables globalTemp N.:| []) globalVariables globalTemp M.empty M.empty S.empty (LiteralBlock literalsBlock H.empty)

data SimpleAssignment = SimpleAssignment { assignmentName :: Text,
                                           assignmentExpr :: Expr
                                         } deriving (Eq,Show)

data ArrayAssignment = ArrayAssignment { arrayAssigmnentName    :: Text,
                                         arrayAssignmentIndices :: NonEmpty Expr,
                                         arrayAssignmentExpr    :: Expr
                                       } deriving (Eq,Show)

data ObjectAssignment = ObjectAssignment { objectAssignmentName   :: Text,
                                           objectAssignmentMember :: Text,
                                           objectAssignmentExpr   :: Expr
                                         } deriving (Eq,Show)

data MainProgramElement =
    MainProgramClass Class
  | MainProgramFunction Function
  | MainProgramStatement Statement deriving (Eq,Show)

newtype MainProgram = MainProgram { mainProgramElements :: NonEmpty MainProgramElement
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
                                           argumentType :: SimpleType,
                                           argumentAddress :: Address
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
                                                             classConstructorParameterType :: ComposedType
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
                                         classDefinitionMethods         :: M.Map Text FunctionDefinition
                                       } deriving (Eq,Show)

data Op = Sum | Minus | Times | Div | Exp | Eq | NEq | Lt | Gt | Lte | Gte | And | Or deriving (Eq,Show)

newtype Address = Address Int deriving (Eq,Show)

data Expr = Expr { innerExpression :: SimpleExpr,
                   expressionType  :: ComposedType,
                   memoryAddress   :: Address
                 } deriving (Eq,Show)

data SimpleExpr =
    Var Text
  | IntLiteral Int Address
  | FloatLiteral Double Address
  | BoolLiteral Bool Address
  | FunctionCallExpr FunctionCall
  | MethodCallExpr MethodCall
  | MemberAccess Text Text
  | Not SimpleExpr
  | Neg SimpleExpr
  | StringLiteral String Address
  | CharLiteral Char Address
  | Operate Op SimpleExpr SimpleExpr
  | FloatConversion SimpleExpr
  | ArrayAccess Text (NonEmpty Expr) deriving (Eq, Show)

data WhileLoop = WhileLoop { whileCondition    :: Expr,
                             whileConditionEnd :: Int,
                             whileStatements   :: NonEmpty Statement
                           } deriving (Eq,Show)

data ForLoop = ForLoop { forDeclaration     :: NonEmpty Text,
                         forDeclarationType :: SimpleType,
                         forDeclarationExpr :: Expr,
                         forCondition       :: Expr,
                         forConditionEnd    :: Int,
                         forAssignmentStart :: Int,
                         forAssignment      :: NonEmpty SimpleAssignment,
                         forStatements      :: NonEmpty Statement
                        } deriving (Eq,Show)

data ConditionalBlock = ConditionalBlock { conditionalStart :: Int,
                                           conditionalExpr :: Expr,
                                           conditionalEnd :: Int,
                                           conditionalStatements :: NonEmpty Statement
                                         } deriving (Eq,Show)

data Conditional = Conditional { ifBlock    :: ConditionalBlock,
                                 elifBlocks :: [ConditionalBlock],
                                 elseBlock  :: Maybe (NonEmpty Statement)
                               } deriving (Eq,Show)

data CreateObject = CreateObject {  createObjectVariableName :: Text,
                                    createObjectClassName    :: Text,
                                    createObjectExpressions  :: [Expr]
                                } deriving (Eq,Show)
