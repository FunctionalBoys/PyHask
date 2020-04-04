module ParserTypes where

import           Data.Text       (Text)
import           Data.Void
import           Text.Megaparsec

type Parser = Parsec Void Text
