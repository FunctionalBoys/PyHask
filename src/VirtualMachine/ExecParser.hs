{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module VirtualMachine.ExecParser (parseExecutable) where

import           Control.Monad
import           Control.Monad.Combinators.NonEmpty
import           Data.Bifunctor
import qualified Data.Map.Strict                    as M
import           Data.Text                          (Text)
import qualified Data.Vector                        as V
import           Data.Void
import           GHC.Float
import           Text.Megaparsec                    hiding (some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L
import           VirtualMachine.VMTypes

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = try (lexeme L.decimal) <?> "Integer literal"

float :: Parser Double
float = try (lexeme L.float) <?> "Float literal"

bool :: Parser Bool
bool = True <$ symbol "True" <|> False <$ symbol "False"

charP :: Parser Char
charP = lexeme L.charLiteral

nullP :: Parser ()
nullP = void $ symbol "null"

address :: Parser Address
address = try (lexeme L.decimal) <?> "Memory address"

pointer :: Parser Pointer
pointer = try (lexeme L.decimal) <?> "Instruction pointer"

parseExecutable :: String -> Text -> Either String ParserResult
parseExecutable filename input = first errorBundlePretty $ runParser (between space eof execParser) filename input

functionName :: Parser String
functionName = (lexeme . try) p <?> "function name"
  where
    p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '.')

execParser :: Parser ParserResult
execParser = ParserResult <$> memoryBoundsParser <*> literalMemoryBoundsParser <*> parseLiterals <*> (M.fromList <$> many nameDefinitionParser) <*> some instructionParser

nameDefinitionParser :: Parser (String, FunctionDefinition)
nameDefinitionParser = try $ (,) <$> functionName <*> functionDefinitionParser

parseLiterals :: Parser [(TypeWrapper, Address)]
parseLiterals = do
  literalCount <- integer
  count literalCount valueAddressParser

functionDefinitionParser :: Parser FunctionDefinition
functionDefinitionParser = do
  instructionStart <- pointer
  functionBounds <- memoryBoundsParser
  paramCount <- integer
  let emptyParams = V.replicate paramCount 0
  params <- count paramCount parameter
  let parameterAddress = emptyParams V.// params
  return FunctionDefinition{..}
  where
    parameter = (,) <$> integer <*> address

gotoParser :: Parser Instruction
gotoParser = GOTO <$> (symbol "GOTO" *> nullP *> nullP *> pointer) <?> "GOTO instruction"

gototParser :: Parser Instruction
gototParser = GOTOT <$> (symbol "GOTOT" *> address <* nullP) <*> pointer <?> "GOTOT instruction"

gotofParser :: Parser Instruction
gotofParser = GOTOF <$> (symbol "GOTOF" *> address <* nullP) <*> pointer <?> "GOTOF instruction"

assign :: Parser Instruction
assign = Assign <$> (symbol "Assign" *> address <* nullP) <*> pointer

verify :: Parser Instruction
verify = Verify <$> (symbol "ArrayVerify" *> address) <*> address <*> address

arrayAccess :: Parser Instruction
arrayAccess = ArrayAccess <$> (symbol "ArrayAccess" *> address) <*> address <*> address

arrayAssign :: Parser Instruction
arrayAssign = ArrayAssign <$> (symbol "ArrayAssign" *> address) <*> address <*> address

programEnd :: Parser Instruction
programEnd = ProgramEnd <$ (symbol "End" *> nullP *> nullP *> nullP)

noOp :: Parser Instruction
noOp = NoOp <$ (symbol "NoOp" *> nullP *> nullP *> nullP)

eraParser :: Parser Instruction
eraParser = Era <$> (symbol "Era" *> functionName)

gosubParser :: Parser Instruction
gosubParser = GOSUB <$> (symbol "GOSUB" *> functionName)

endFuncParser :: Parser Instruction
endFuncParser = EndFunc <$ (symbol "EndFunc" *> nullP *> nullP *> nullP)

funcParamParser :: Parser Instruction
funcParamParser = FuncParam <$> (symbol "FuncParam" *> address) <*> (nullP *> integer)

sumParser :: Parser Operation
sumParser = f <$ symbol "Sum"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ IntWrapper (left + right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ FloatWrapper (left + right)
    f _ _ = Nothing

minusParser :: Parser Operation
minusParser = f <$ symbol "Minus"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ IntWrapper (left - right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ FloatWrapper (left - right)
    f _ _ = Nothing

timesParser :: Parser Operation
timesParser = f <$ symbol "Times"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ IntWrapper (left * right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ FloatWrapper (left * right)
    f _ _ = Nothing

divParser :: Parser Operation
divParser = f <$ symbol "Div"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ IntWrapper (left `div` right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ FloatWrapper (left / right)
    f _ _ = Nothing

expParser :: Parser Operation
expParser = f <$ symbol "Exp"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ FloatWrapper (int2Double left ** int2Double right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ FloatWrapper (left ** right)
    f _ _ = Nothing

eqParser :: Parser Operation
eqParser = f <$ symbol "Eq"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ BoolWrapper(left == right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ BoolWrapper (left == right)
    f _ _ = Nothing

neqParser :: Parser Operation
neqParser = f <$ symbol "NEq"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ BoolWrapper(left /= right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ BoolWrapper (left /= right)
    f _ _ = Nothing

ltParser :: Parser Operation
ltParser = f <$ symbol "Lt"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ BoolWrapper(left < right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ BoolWrapper (left < right)
    f _ _ = Nothing

gtParser :: Parser Operation
gtParser = f <$ symbol "Gt"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ BoolWrapper(left > right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ BoolWrapper (left > right)
    f _ _ = Nothing

lteParser :: Parser Operation
lteParser = f <$ symbol "Lte"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ BoolWrapper(left <= right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ BoolWrapper (left <= right)
    f _ _ = Nothing

gteParser :: Parser Operation
gteParser = f <$ symbol "Gte"
  where
    f (IntWrapper left) (IntWrapper right) = Just $ BoolWrapper(left >= right)
    f (FloatWrapper left) (FloatWrapper right) = Just $ BoolWrapper (left >= right)
    f _ _ = Nothing

andParser :: Parser Operation
andParser = f <$ symbol "And"
  where
    f (BoolWrapper left) (BoolWrapper right) = Just $ BoolWrapper(left && right)
    f _ _ = Nothing

orParser :: Parser Operation
orParser = f <$ symbol "Or"
  where
    f (BoolWrapper left) (BoolWrapper right) = Just $ BoolWrapper(left || right)
    f _ _ = Nothing

notParser :: Parser UnaryOperation
notParser = f <$ symbol "Not"
  where
    f (BoolWrapper center) = Just $ BoolWrapper(not center)
    f _                    = Nothing

negParser :: Parser UnaryOperation
negParser = f <$ symbol "Neg"
  where
    f (IntWrapper center)   = Just $ IntWrapper(center * (-1))
    f (FloatWrapper center) = Just $ FloatWrapper(center * (-1))
    f _                     = Nothing

floatConvertionParser :: Parser UnaryOperation
floatConvertionParser = f <$ symbol "FloatConvert"
  where
    f (IntWrapper center)   = Just $ FloatWrapper(int2Double center)
    f (FloatWrapper center) = Just $ FloatWrapper center
    f _                     = Nothing

operationParser :: Parser Operation
operationParser = choice [sumParser, minusParser, timesParser, divParser, expParser, eqParser, neqParser, lteParser, gteParser, ltParser, gtParser, andParser, orParser]

unaryParser :: Parser UnaryOperation
unaryParser = choice [notParser, negParser, floatConvertionParser]

unaryOperationParser :: Parser Instruction
unaryOperationParser = UnaryOperation <$> unaryParser <*> (address <* nullP) <*> address

binaryOperationParser :: Parser Instruction
binaryOperationParser = BinaryOperation <$> operationParser <*> address <*> address <*> address

printParser :: Parser Instruction
printParser = Print <$> (symbol "Print" *> address <* nullP <* nullP)

memberAccessParser :: Parser Instruction
memberAccessParser = MemberAccess <$> (symbol "MemberAccess" *> functionName) <*> address <*> address

memberAssignParser :: Parser Instruction
memberAssignParser = MemberAssign <$> (symbol "MemberAssign" *> functionName) <*> address <*> address

nextObjectIdParser :: Parser Instruction
nextObjectIdParser = NextObjectId <$> (symbol "NextObjectId" *> nullP *> nullP *> address)

instructionParser :: Parser Instruction
instructionParser = choice [binaryOperationParser, unaryOperationParser, gototParser, gotofParser, assign, verify, arrayAccess, arrayAssign, endFuncParser, programEnd, noOp, gotoParser, printParser, eraParser, gosubParser, funcParamParser, memberAccessParser, memberAssignParser, nextObjectIdParser] <?> "executable instruction"

typeBoundParser :: Parser TypeBounds
typeBoundParser = TypeBounds <$> integer <*> (integer *> integer) <*> integer <*> (integer *> integer)

typeWrapperParser :: Parser TypeWrapper
typeWrapperParser = FloatWrapper <$> float <|> IntWrapper <$> integer <|> BoolWrapper <$> bool <|> CharWrapper <$> charP

valueAddressParser :: Parser (TypeWrapper, Address)
valueAddressParser = try $ (,) <$> typeWrapperParser <*> address

memoryBoundsParser :: Parser MemoryBounds
memoryBoundsParser = MemoryBounds <$> typeBoundParser <*> typeBoundParser <*> typeBoundParser <*> typeBoundParser

literalTypeBoundsParser :: Parser TypeBounds
literalTypeBoundsParser = do
  lower <- address
  _ <- address
  upper <- address
  return $ TypeBounds lower upper (upper + 1) (upper + 1)

literalMemoryBoundsParser :: Parser MemoryBounds
literalMemoryBoundsParser = MemoryBounds <$> literalTypeBoundsParser <*> literalTypeBoundsParser <*> literalTypeBoundsParser <*> literalTypeBoundsParser
