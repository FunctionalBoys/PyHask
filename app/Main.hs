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

writeLines :: (Foldable t) => String -> t String -> Handle -> IO ()
writeLines line linez handle = do
  hPutStrLn handle line
  forM_ linez (hPutStrLn handle)

printCompilation :: FilePath -> ParserState -> IO ()
printCompilation filename ParserState{quadruplesSequence=quads, globalVariablesBlock=globalVars, globalTempBlock=globalTempVars, literalBlock=literals} = do
  let gMemories = displayVarTmpMemoryBlock globalVars globalTempVars
  let lBlock = stringifyLiteralBlock literals
  let eLines = mapM checkPlaceholder quads
  either putStrLn (f $ gMemories <> lBlock) eLines
  where
    f gMemories linez = withFile (takeBaseName filename ++ ".phc") WriteMode (writeLines gMemories linez)

main :: IO ()
main = do
  actionData <- execParser opts
  case actionData of
    CompileAction filename -> do
      input <- T.readFile filename
      either putStrLn (printCompilation filename) $ parseProgram filename input
    ExecuteAction _ -> putStrLn "Execution not yet supported"
