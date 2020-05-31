{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.List.NonEmpty              as N
import qualified Data.Map.Strict                 as M
import           Data.Text                       (Text, unpack)
import qualified Data.Text.IO                    as T
import qualified Data.Vector                     as V
import           Options.Applicative
import           Parser.ExecutableCreator
import           Parser.Parser
import           Parser.ParserTypes              hiding (Parser)
import           System.FilePath                 (takeBaseName)
import           System.IO
import           VirtualMachine.ExecParser
import           VirtualMachine.StateInitializer
import           VirtualMachine.VMExecution
import           VirtualMachine.VMTypes

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
printCompilation filename ParserState{..} = do
  let gMemories = displayVarTmpMemoryBlock globalVariablesBlock globalTempBlock
  let lBlock = stringifyLiteralBlock literalBlock
  let eLines = mapM checkPlaceholder quadruplesSequence
  let fLines = concat $ g <$> M.toList functionDefinitions
  either putStrLn (f $ gMemories <> lBlock <> fLines) eLines
  where
    f gMemories linez = withFile (takeBaseName filename ++ ".phc") WriteMode (writeLines gMemories linez)
    g (fName, fDefinition) = unpack fName <> " " <> stringifyFunctionDefinition fDefinition

parseExec :: String -> Text -> Either String (IO (Either String ()))
parseExec filename input = do
  parserResult@ParserResult{instructions=instructionList} <- parseExecutable filename input
  mState <- createVirtualMachineState parserResult
  let instructionVector = V.fromList $ N.toList instructionList
  return $ runExceptT $ evalStateT (runReaderT virtualMachine instructionVector) mState

main :: IO ()
main = do
  actionData <- execParser opts
  case actionData of
    CompileAction filename -> do
      input <- T.readFile filename
      either putStrLn (printCompilation filename) $ parseProgram filename input
    ExecuteAction filename -> do
      input <- T.readFile filename
      let parsedResult = parseExec filename input
      case parsedResult of
        Left err -> putStrLn err
        Right vmAction -> do
          vmResult <- vmAction
          either putStrLn return vmResult
