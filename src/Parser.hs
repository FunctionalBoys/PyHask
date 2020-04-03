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

ifSymbol :: Parser ()
ifSymbol = reservedWord "if"

elifSymbol :: Parser ()
elifSymbol = reservedWord "elif"

elseSymbol :: Parser ()
elseSymbol = reservedWord "else"

plusSymbol :: Parser ()
plusSymbol = void $ symbol "+"

minusSymbol :: Parser ()
minusSymbol = void $ symbol "-"

divisionSymbol :: Parser ()
divisionSymbol = void $ symbol "/"

timesSymbol :: Parser ()
timesSymbol = void $ symbol "*"

equalSymbol :: Parser ()
equalSymbol = void $ symbol ":="

isEqualSymbol :: Parser ()
isEqualSymbol = void $ symbol "=="

lessSymbol :: Parser ()
lessSymbol = void $ symbol "<"

greaterSymbol :: Parser ()
greaterSymbol = void $ symbol ">"

differentSymbol :: Parser ()
differentSymbol = void $ symbol "!="

lessEqSymbol :: Parser ()
lessEqSymbol = void $ symbol "<="

greaterEqSymbol :: Parser ()
greaterEqSymbol = void $ symbol ">="

andSymbol :: Parser ()
andSymbol = reservedWord "and"

orSymbol :: Parser ()
orSymbol = reservedWord "or"

notSymbol :: Parser ()
notSymbol = reservedWord "not"

commaSymbol :: Parser ()
commaSymbol = void $ symbol ","

dotSymbol :: Parser ()
dotSymbol = void $ symbol "."

voidSymbol :: Parser ()
voidSymbol = reservedWord "void"

printSymbol :: Parser ()
printSymbol = reservedWord "print"

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
