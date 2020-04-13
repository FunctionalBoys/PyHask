module Main where

import qualified Data.Text.IO        as T
import           Options.Applicative
import           Parser

newtype Filename = Filename { name :: String }

fileArg :: Parser Filename
fileArg = Filename <$> argument str (metavar "FILE")

opts :: ParserInfo Filename
opts = info (fileArg <**> helper)
  (fullDesc
   <> progDesc "Parse a PyHask file"
   <> header "PyHask parser")

main :: IO ()
main = do
  fileData <- execParser opts
  let filename = name fileData
  input <- T.readFile filename
  either putStrLn print $ parseProgram filename input
