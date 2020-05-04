module Main where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Sequence            (Seq)
import qualified Data.Text.IO             as T
import           Options.Applicative
import           Parser
import           ParserTypes              (MainProgram, Quad)

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

printResult :: (MainProgram, Seq Quad) -> IO ()
printResult (mainProgram, quads) = do
  putStrLn "Printing syntax tree"
  print mainProgram
  putStrLn "Printing quads"
  evalStateT (forM_ quads indexedPrint) 0

main :: IO ()
main = do
  fileData <- execParser opts
  let filename = name fileData
  input <- T.readFile filename
  either putStrLn printResult $ parseProgram filename input
