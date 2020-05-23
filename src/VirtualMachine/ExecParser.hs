module VirtualMachine.ExecParser where

import           Control.Monad
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L
import           VirtualMachine.VMTypes

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

operationParser :: Parser Operation
operationParser = choice [sumParser]

binaryOperationParser :: Parser Instruction
binaryOperationParser = BinaryOperation <$> operationParser <*> address <*> address <*> address

instrunctionParser :: Parser Instruction
instrunctionParser = choice [gotoParser, binaryOperationParser]
