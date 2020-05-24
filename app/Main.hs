module Main where

import           Control.Monad
import qualified Data.Text.IO             as T
import           Options.Applicative
import           Parser.ExecutableCreator
import           Parser.Parser
import           Parser.ParserTypes       hiding (Parser)
import           System.FilePath          (takeBaseName)
import           System.IO

data ActionFilename = CompileAction String | ExecuteAction String

compileArg :: Parser ActionFilename
compileArg = CompileAction <$> argument str (metavar "COMPILE_FILE")

executeArg :: Parser ActionFilename
executeArg = ExecuteAction <$> argument str (metavar "EXECUTE_FILE")

actionOpts :: Parser ActionFilename
actionOpts = subparser $
  command "compile" (info (compileArg <**> helper) (progDesc "Compile a program"))
  <> command "execute" (info (executeArg <**> helper) (progDesc "Execute a program"))

opts :: ParserInfo ActionFilename
opts = info (actionOpts <**> helper)
  (fullDesc
   <> progDesc "Compile or execute a PyHask compiled file"
   <> header "PyHask compiler and virtual machine")

writeLines :: (Foldable t) => t String -> Handle -> IO ()
writeLines linez handle = forM_ linez (hPutStrLn handle)

printCompilation :: FilePath -> ParserState -> IO ()
printCompilation filename pState = do
  let quads = quadruplesSequence pState
  let eLines = mapM checkPlaceholder quads
  either putStrLn f eLines
  where
    f linez = withFile (takeBaseName filename ++ ".phc") WriteMode (writeLines linez)

main :: IO ()
main = do
  actionData <- execParser opts
  case actionData of
    CompileAction filename -> do
      input <- T.readFile filename
      either putStrLn (printCompilation filename) $ parseProgram filename input
    ExecuteAction _ -> putStrLn "Execution not yet supported"
