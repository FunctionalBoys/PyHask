module VirtualMachine.VMExecution (virtualMachine) where

import           Control.Category                ((<<<))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as M
import qualified Data.Sequence                   as S
import qualified Data.Vector                     as V
import           VirtualMachine.ConversionUtils
import           VirtualMachine.StateInitializer
import           VirtualMachine.VMTypes
import VirtualMachine.ExecParser (typeWrapperParser)
import qualified Data.Text.IO as T
import Text.Megaparsec

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ predicate action = go
  where
    go = do
      condition <- predicate
      when condition (action *> go)

nextInstruction :: VirtualMachine Instruction
nextInstruction = do
  currentPointer <- gets instructionPointer
  mInstruction <- asks $ (V.!? currentPointer) <<< machineInstructions
  maybe (throwError "Instruction pointer out of bounds") return mInstruction

executionCondition :: VirtualMachine Bool
executionCondition = isNotEnd <$> nextInstruction
  where
    isNotEnd ProgramEnd = False
    isNotEnd _          = True

increaseInstructionPointer :: Int -> MachineState -> MachineState
increaseInstructionPointer increase mState@MachineState{instructionPointer=pointer} = mState{instructionPointer=pointer+increase}

executionAction :: VirtualMachine ()
executionAction = do
  currentInstruction <- nextInstruction
  executeInstruction currentInstruction
  modify $ increaseInstructionPointer 1

jump :: Pointer -> VirtualMachine ()
jump index = modify (\mState -> mState{instructionPointer = index - 1})

readValue :: VirtualMachine TypeWrapper
readValue = do
  input <- liftIO T.getLine
  let readResult = runParser typeWrapperParser "" input
  either (const $ throwError "Error reading from terminal") return readResult

executeInstruction :: Instruction -> VirtualMachine ()
executeInstruction ProgramEnd = throwError "Program end should be unreachable by execution loop"
executeInstruction (Assign source destiny) = do
  value <- getValueFromAddress source
  setValue value destiny
executeInstruction NoOp = return ()
executeInstruction (BinaryOperation op left right destiny) = do
  leftValue <- getValueFromAddress left
  rightValue <- getValueFromAddress right
  let mValue = leftValue `op` rightValue
  value <- liftEither $ maybe (Left "Cannot operate on addresses") Right mValue
  setValue value destiny
executeInstruction (UnaryOperation op center destiny) = do
  centerValue <- getValueFromAddress center
  let mValue = op centerValue
  value <- liftEither $ maybe (Left "Cannot operate on addresses") Right mValue
  setValue value destiny
executeInstruction (GOTO index) = jump index
executeInstruction (GOTOT address index) = do
  value <- getValueFromAddress address
  when (value == BoolWrapper True) (jump index)
executeInstruction (GOTOF address index) = do
  value <- getValueFromAddress address
  when (value == BoolWrapper False) (jump index)
executeInstruction (Verify valueAddress lowerAddress upperAddress) = do
  value <- getValueFromAddress valueAddress
  lower <- getValueFromAddress lowerAddress
  upper <- getValueFromAddress upperAddress
  case (value, lower, upper) of
    (IntWrapper index, IntWrapper lowerBound, IntWrapper upperBound) -> when (index < lowerBound || index > upperBound) (throwError "Out of bounds array during verification")
    _ -> throwError "Wrong datatypes for array verification"
executeInstruction (ArrayAccess arrayBase offset destiny) = do
  offsetW <- getValueFromAddress offset
  case offsetW of
    IntWrapper offsetValue -> do
      arrayValue <- getValueFromAddress $ arrayBase + offsetValue
      setValue arrayValue destiny
    _ -> throwError "No integer offset for array access"
executeInstruction (ArrayAssign arrayBase offset valueAddress) = do
  offsetW <- getValueFromAddress offset
  case offsetW of
    IntWrapper offsetValue -> do
      value <- getValueFromAddress valueAddress
      setValue value (arrayBase + offsetValue)
    _ -> throwError "No integer offset for array assignment"
executeInstruction (Print address) = do
  value <- getValueFromAddress address
  case value of
    IntWrapper integer -> liftIO $ print integer
    FloatWrapper float -> liftIO $ print float
    CharWrapper char   -> liftIO $ putStrLn [char]
    BoolWrapper bool   -> liftIO $ print bool
executeInstruction (Read address) = do
  newValue <- readValue
  setValue newValue address
executeInstruction EndFunc = do
  LocalContext{checkpoint=recoveredPointer} <- removeLocalContext
  jump recoveredPointer
executeInstruction (Era fName) = do
  FunctionDefinition{functionBounds=bounds} <- getFunctionDefinition fName
  let wContext = LocalContext 0 (createMemBlock bounds) (addressInfo bounds)
  modify (\mState -> mState{workingContext=Working fName wContext})
executeInstruction (GOSUB fName) = do
  FunctionDefinition{instructionStart=start} <- getFunctionDefinition fName
  wContext <- gets workingContext
  currentInstruction <- gets instructionPointer
  case wContext of
    NoContext -> throwError "Trying to jump to a function without providing context for it before"
    Working _ context -> jump start *> modify (addLocalContext context{checkpoint=currentInstruction+1})
executeInstruction (FuncParam address index) = do
  wContext <- gets workingContext
  (fName, oldContext@LocalContext{addressType=addressConvertor, localMemory=oldMemory}) <- case wContext of
    NoContext -> throwError "Trying to add param without previously using Era"
    Working name context -> return (name, context)
  value <- getValueFromAddress address
  FunctionDefinition{parameterAddress=paramVector} <- getFunctionDefinition fName
  paramAddress <- maybe (throwError "Parameter out of range") return (paramVector V.!? index)
  let (_,bounds) = addressConvertor paramAddress
  memoryIndex <- liftEither $ addressConversion bounds paramAddress
  let nMemory = updateMemoryBlock memoryIndex value oldMemory
  modify (\mState -> mState{workingContext = Working fName oldContext{localMemory=nMemory}})
executeInstruction (NextObjectId address) = do
  nextId <- state (\mState@MachineState{objects=objs, objectIdCounter=counter} -> (counter, mState{objects=objs S.|> M.empty, objectIdCounter=counter+1}))
  setValue (IntWrapper nextId) address
executeInstruction (MemberAccess member base destination) = do
  objectId <- getValueFromAddress base >>= forceInteger
  object <- getObjectByIndex objectId
  value <- maybe (throwError $ "Object doesn't have member " <> member) return (M.lookup member object)
  setValue value destination
executeInstruction (MemberAssign member base origin) = do
  objectId <- getValueFromAddress base >>= forceInteger
  object <- getObjectByIndex objectId
  originValue <- getValueFromAddress origin
  let newObject = M.insert member originValue object
  modify (\mState@MachineState{objects=objs} -> mState{objects=S.update objectId newObject objs})

virtualMachine :: VirtualMachine ()
virtualMachine = whileM_ executionCondition executionAction

getLocalContext :: VirtualMachine LocalContext
getLocalContext = do
  contexts <- gets localContexts
  case contexts of
    context : _ -> return context
    []          -> throwError "Not inside a local context"

getFunctionDefinition :: String -> VirtualMachine FunctionDefinition
getFunctionDefinition fName = do
  mDefinition <- asks (M.lookup fName <<< functionDefinitions)
  maybe (throwError $ "No function with name " <> fName) return mDefinition

getObjectByIndex :: Int -> VirtualMachine (Map String TypeWrapper)
getObjectByIndex index = do
  mObject <- gets (S.lookup index <<< objects)
  maybe (throwError $ "No object with id " <> show index) return mObject

addressContextType :: Address -> ContextType
addressContextType address
  | address >= 1 && address <= 12000 = Global
  | address >= 12001 && address <= 44000 = Local
  | otherwise = Static

addLocalContext :: LocalContext -> MachineState -> MachineState
addLocalContext context mState@MachineState{localContexts=contexts} = mState{localContexts=context:contexts}

removeLocalContext :: VirtualMachine LocalContext
removeLocalContext = do
  contexts <- gets localContexts
  case contexts of
    [] -> throwError "Not in a local context"
    context : restOfContexts -> context <$ modify (\mState -> mState{localContexts=restOfContexts})

updateContext :: ContextType -> LocalContext -> MachineState -> MachineState
updateContext Global context mState = mState{globalContext = context}
updateContext Static context mState = mState{staticContext = context}
updateContext Local context mState@MachineState{localContexts=_ : xs} = mState{localContexts = context : xs}
updateContext _ _ mState = mState

getContext :: ContextType -> VirtualMachine LocalContext
getContext Local  = getLocalContext
getContext Global = gets globalContext
getContext Static = gets staticContext

addressContext :: Address -> VirtualMachine LocalContext
addressContext  = getContext <<< addressContextType

forceInteger :: TypeWrapper -> VirtualMachine Int
forceInteger (IntWrapper integer) = return integer
forceInteger _ = throwError "Could not get an integer value"

getValueFromAddress :: Address -> VirtualMachine TypeWrapper
getValueFromAddress address = do
  LocalContext{localMemory=memory, addressType=addressConvertor} <- addressContext address
  let (addressT, bounds) = addressConvertor address
  index <- liftEither $ addressConversion bounds address
  liftEither $ maybe (Left "Address out of type bounds") Right $ getValueFromIndex addressT memory index

setValue :: TypeWrapper -> Address -> VirtualMachine ()
setValue value address = do
  let contextType = addressContextType address
  localContext@LocalContext{localMemory=memory, addressType=addressConvertor} <- addressContext address
  let (_, bounds) = addressConvertor address
  index <- liftEither $ addressConversion bounds address
  let nMemory = updateMemoryBlock index value memory
  modify $ updateContext contextType localContext{localMemory=nMemory}
