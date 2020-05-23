module Main where

import           Control.Monad
import           Control.Monad.State.Lazy
import qualified Data.Text.IO             as T
import           Options.Applicative
import           Parser.Parser
import           Parser.ParserTypes       hiding (Parser)

newtype Filename = Filename { name :: String }

fileArg :: Parser Filename
fileArg = Filename <$> argument str (metavar "FILE")

opts :: ParserInfo Filename
opts = info (fileArg <**> helper)
  (fullDesc
   <> progDesc "Parse a PyHask file"
   <> header "PyHask parser")

indexedPrint :: (Show a) => a -> StateT Int IO ()
indexedPrint a = do
  index <- get
  liftIO $ putStr $ show index ++ ". "
  liftIO $ print a
  put (index + 1)

printResult :: (MainProgram, ParserState) -> IO ()
printResult (mainProgram, pState) = do
  putStrLn "Printing syntax tree"
  print mainProgram
  putStrLn "Printing function definitions"
  print (functionDefinitions pState)
  putStrLn "Printing quads"
  let quads = quadruplesSequence pState
  evalStateT (forM_ quads indexedPrint) 0

main :: IO ()
main = do
  fileData <- execParser opts
  let filename = name fileData
  input <- T.readFile filename
  either putStrLn printResult $ parseProgram filename input
