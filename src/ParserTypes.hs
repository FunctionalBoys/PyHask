module ParserTypes where

import           Data.Text       (Text)
import           Data.Void
import           Text.Megaparsec

type Parser = Parsec Void Text

data SimpleType = IntType | FloatType | BoolType | StringType | CharType deriving (Eq,Show)


data ReturnType = ValueReturn SimpleType | VoidReturn


