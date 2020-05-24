module Main where

import           Control.Monad
import qualified Data.Text.IO             as T
import           Options.Applicative
import           Parser.ExecutableCreator
import           Parser.Parser
import           Parser.ParserTypes       hiding (Parser)
import           System.FilePath          (takeBaseName)
import           System.IO

newtype Filename = Filename { name :: String }

fileArg :: Parser Filename
fileArg = Filename <$> argument str (metavar "FILE")

opts :: ParserInfo Filename
opts = info (fileArg <**> helper)
  (fullDesc
   <> progDesc "Parse a PyHask file"
   <> header "PyHask parser")

writeLines :: (Foldable t) => t String -> Handle -> IO ()
writeLines linez handle = forM_ linez (hPutStrLn handle)

printResult :: FilePath -> (MainProgram, ParserState) -> IO ()
printResult filename (_, pState) = do
  let quads = quadruplesSequence pState
  let eLines = mapM checkPlaceholder quads
  either putStrLn f eLines
  where
    f linez = withFile (takeBaseName filename ++ ".phc") WriteMode (writeLines linez)

main :: IO ()
main = do
  fileData <- execParser opts
  let filename = name fileData
  input <- T.readFile filename
  either putStrLn (printResult filename) $ parseProgram filename input
