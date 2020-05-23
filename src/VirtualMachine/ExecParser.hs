module VirtualMachine.ExecParser where

import           Control.Monad
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L
import           VirtualMachine.VMTypes
import GHC.Float

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
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
    f (IntWrapper left) (IntWrapper right) = Just $ FloatWrapper (int2Double(left) ** int2Double(right))
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
    f _ = Nothing

negParser :: Parser UnaryOperation
negParser = f <$ symbol "Neg"
  where 
    f (IntWrapper center) = Just $ IntWrapper(center * (-1))
    f (FloatWrapper center) = Just $ FloatWrapper(center * (-1))
    f _ = Nothing

floatConvertionParser :: Parser UnaryOperation
floatConvertionParser = f <$ symbol "FloatConvert"
  where
    f (IntWrapper center) = Just $ FloatWrapper(int2Double(center))
    f (FloatWrapper center) = Just $ FloatWrapper(center)
    f _ = Nothing

operationParser :: Parser Operation
operationParser = choice [sumParser]

binaryOperationParser :: Parser Instruction
binaryOperationParser = BinaryOperation <$> operationParser <*> address <*> address <*> address

instrunctionParser :: Parser Instruction
instrunctionParser = choice [gotoParser, binaryOperationParser]
