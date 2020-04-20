{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           ParserTypes
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

reservedWords :: [Text]
reservedWords = ["if", "elif", "else", "for", "while", "let", "def", "class", "self", "True", "False", "continue", "pass", "break",
                 "and", "or", "not", "print", "read", "int", "float", "void", "string", "char", "super", "init", "main", "return", "create"]

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reservedWord :: Text -> Parser ()
reservedWord w = try(string w *> notFollowedBy alphaNumChar *> sc)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

mainSymbol :: Parser ()
mainSymbol = reservedWord "main"

ifSymbol :: Parser ()
ifSymbol = reservedWord "if"

elifSymbol :: Parser ()
elifSymbol = reservedWord "elif"

elseSymbol :: Parser ()
elseSymbol = reservedWord "else"

plusSymbol :: Parser Op
plusSymbol = Sum <$ symbol "+"

minusSymbol :: Parser Op
minusSymbol = Minus <$ symbol "-"

divisionSymbol :: Parser Op
divisionSymbol = Div <$ symbol "/"

timesSymbol :: Parser Op
timesSymbol = Times <$ symbol "*"

exponentSymbol :: Parser Op
exponentSymbol = Exp <$ symbol "**"

equalSymbol :: Parser ()
equalSymbol = void $ symbol ":="

isEqualSymbol :: Parser Op
isEqualSymbol = Eq <$ symbol "=="

lessSymbol :: Parser Op
lessSymbol = Lt <$ symbol "<"

greaterSymbol :: Parser Op
greaterSymbol = Gt <$ symbol ">"

differentSymbol :: Parser Op
differentSymbol = NEq <$ symbol "!="

lessEqSymbol :: Parser Op
lessEqSymbol = Lte <$ symbol "<="

greaterEqSymbol :: Parser Op
greaterEqSymbol = Gte <$ symbol ">="

andSymbol :: Parser Op
andSymbol = And <$ reservedWord "and"

orSymbol :: Parser Op
orSymbol = Or <$ reservedWord "or"

notSymbol :: Parser ()
notSymbol = reservedWord "not"

commaSymbol :: Parser ()
commaSymbol = void $ symbol ","

dotSymbol :: Parser ()
dotSymbol = void $ symbol "."

intSymbol :: Parser SimpleType
intSymbol = IntType <$ reservedWord "int"

boolSymbol :: Parser SimpleType
boolSymbol = BoolType <$ reservedWord "bool"

floatSymbol :: Parser SimpleType
floatSymbol = FloatType <$ reservedWord "float"

charSymbol :: Parser SimpleType
charSymbol = CharType <$ reservedWord "char"

stringSymbol :: Parser SimpleType
stringSymbol = StringType <$ reservedWord "string"

voidSymbol :: Parser ReturnType
voidSymbol = VoidReturn <$ reservedWord "void"

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

initSymbol :: Parser ()
initSymbol = reservedWord "init"

superSymbol :: Parser ()
superSymbol = reservedWord "super"

selfSymbol :: Parser Text
selfSymbol = "self" <$ reservedWord "self"

defSymbol :: Parser ()
defSymbol = reservedWord "def"

arrowSymbol :: Parser ()
arrowSymbol = void $ symbol "->"

returnSymbol :: Parser ()
returnSymbol = reservedWord "return"

letSymbol :: Parser ()
letSymbol = reservedWord "let"

continueSymbol :: Parser Statement
continueSymbol = Continue <$ reservedWord "continue"

passSymbol :: Parser Statement
passSymbol = Pass <$ reservedWord "pass"

breakSymbol :: Parser Statement
breakSymbol = Break <$ reservedWord "break"

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
    p = T.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
    check x = if x `elem` reservedWords
      then fail $ "Reserved word " ++ show x ++ " cannot be an identifier"
      else return x

createSymbol :: Parser ()
createSymbol = reservedWord "create"
