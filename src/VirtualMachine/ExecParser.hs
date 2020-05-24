{-# LANGUAGE OverloadedStrings #-}

module VirtualMachine.ExecParser where

import           Control.Monad
import           Data.Text                  (Text)
import           Data.Void
import           GHC.Float
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           VirtualMachine.VMTypes

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

nullP :: Parser ()
nullP = void $ symbol "null"

address :: Parser Address
address = lexeme L.decimal

pointer :: Parser Pointer
pointer = lexeme L.decimal

gotoParser :: Parser Instruction
gotoParser = GOTO <$> (symbol "GOTO" *> nullP *> nullP *> pointer)

gototParser :: Parser Instruction
gototParser = GOTOT <$> (symbol "GOTOT" *> address) <*> pointer

gotofParser :: Parser Instruction
gotofParser = GOTOF <$> (symbol "GOTOF" *> address) <*> pointer

assign :: Parser Instruction
assign = Assign <$> (symbol "Assign" *> address) <*> pointer

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
    f (BoolWrapper left) (BoolWrapper right) = Just $ BoolWrapper(left == right)
    f _ _ = Nothing

neqParser :: Parser Operation
neqParser = f <$ symbol "NEq"
  where
    f (BoolWrapper left) (BoolWrapper right) = Just $ BoolWrapper(left /= right)
    f _ _ = Nothing

ltParser :: Parser Operation
ltParser = f <$ symbol "Lt"
  where
    f (BoolWrapper left) (BoolWrapper right) = Just $ BoolWrapper(left < right)
    f _ _ = Nothing

gtParser :: Parser Operation
gtParser = f <$ symbol "Gt"
  where
    f (BoolWrapper left) (BoolWrapper right) = Just $ BoolWrapper(left > right)
    f _ _ = Nothing

lteParser :: Parser Operation
lteParser = f <$ symbol "Lte"
  where
    f (BoolWrapper left) (BoolWrapper right) = Just $ BoolWrapper(left <= right)
    f _ _ = Nothing

gteParser :: Parser Operation
gteParser = f <$ symbol "Gte"
  where
    f (BoolWrapper left) (BoolWrapper right) = Just $ BoolWrapper(left >= right)
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
operationParser = choice [sumParser, minusParser, timesParser, divParser, expParser, eqParser, neqParser, ltParser, gtParser, lteParser, gteParser, andParser, orParser]

unaryParser :: Parser UnaryOperation
unaryParser = choice [notParser, negParser, floatConvertionParser]

unaryOperationParser :: Parser Instruction
unaryOperationParser = UnaryOperation <$> unaryParser <*> address <*> address

binaryOperationParser :: Parser Instruction
binaryOperationParser = BinaryOperation <$> operationParser <*> address <*> address <*> address

instrunctionParser :: Parser Instruction
instrunctionParser = choice [gotoParser, binaryOperationParser, unaryOperationParser, gototParser, gotofParser, assign, verify, arrayAccess, arrayAssign, programEnd, noOp]

typeBoundParser :: Parser TypeBounds
typeBoundParser = do
  vL <- address
  vU <- address
  _ <- address
  tL <- address
  tU <- address
  _ <- address
  return (TypeBounds vL vU tL tU)

memoryBoundsParser :: Parser MemoryBounds
memoryBoundsParser = do
  iB <- typeBoundParser
  fB <- typeBoundParser
  cB <- typeBoundParser
  bB <- typeBoundParser
  return (MemoryBounds iB fB cB bB)