{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

reservedWords :: [Text]
reservedWords = ["if", "elif", "else", "for", "while", "let", "def", "class", "True", "False", "continue", "pass", "break", "and", "or", "not", "print", "read"]

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reservedWord :: Text -> Parser ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

ifSymbol :: Parser Text
ifSymbol = symbol "if"

elifSymbol :: Parser Text
elifSymbol = symbol "elif"

elseSymbol :: Parser Text
elseSymbol = symbol "else"

plusSymbol :: Parser Text
plusSymbol = symbol "+"

minusSymbol :: Parser Text
minusSymbol = symbol "-"

divisionSymbol :: Parser Text
divisionSymbol = symbol "/"

timesSymbol :: Parser Text
timesSymbol = symbol "*"

equalSymbol :: Parser Text
equalSymbol = symbol ":="

isEqualSymbol :: Parser Text
isEqualSymbol = symbol "=="

lessSymbol :: Parser Text
lessSymbol = symbol "<"

greaterSymbol :: Parser Text
greaterSymbol = symbol ">"

differentSymbol :: Parser Text
differentSymbol = symbol "!="

lessEqSymbol :: Parser Text
lessEqSymbol = symbol "<="

greaterEqSymbol :: Parser Text
greaterEqSymbol = symbol ">="

andSymbol :: Parser Text 
andSymbol = symbol "and"

orSymbol :: Parser Text
orSymbol = symbol "or"

notSymbol :: Parser Text
notSymbol = symbol "not"

commaSymbol :: Parser Text
commaSymbol = symbol ","

dotSymbol :: Parser Text
dotSymbol = symbol "."

voidSymbol :: Parser Text
voidSymbol = symbol "void"

printSymbol :: Parser Text
printSymbol = symbol "print"

readSymbol :: Parser ()
readSymbol = reservedWord "read"

forSymbol :: Parser ()
forSymbol = reservedWord "for"

whileSymbol :: Parser ()
whileSymbol = reservedWord "while"

colonSymbol :: Parser ()
colonSymbol = void $ symbol ":"

classSymbol :: Parser ()
classSymbol = reservedWord "class"

defSymbol :: Parser ()
defSymbol = reservedWord "def"

arrowSymbol :: Parser ()
arrowSymbol = void $ symbol "->"

letSymbol :: Parser ()
letSymbol = reservedWord "let"

continueSymbol :: Parser ()
continueSymbol = reservedWord "continue"

passSymbol :: Parser ()
passSymbol = reservedWord "pass"

breakSymbol :: Parser ()
breakSymbol = reservedWord "break"

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

floatLiteral :: Parser Double
floatLiteral = lexeme L.float

trueSymbol :: Parser Bool
trueSymbol = True <$ reservedWord "True"

falseSymbol :: Parser Bool
falseSymbol = False <$ reservedWord "False"

stringLiteral :: Parser Text
stringLiteral = char '\'' >> T.pack <$> manyTill alphaNumChar (char '\'')

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x = if x `elem` reservedWords
      then fail $ "Reserved word " ++ show x ++ " cannot be an identifier"
      else return x
