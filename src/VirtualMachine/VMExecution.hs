{-# LANGUAGE RecordWildCards #-}

module VirtualMachine.VMExecution (virtualMachine) where

import           Control.Category
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.Vector              as V
import           VirtualMachine.VMTypes

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ predicate action = go
  where
    go = do
      condition <- predicate
      when condition (action *> go)

nextInstruction :: VirtualMachine Instruction
nextInstruction = do
  currentPointer <- gets instructionPointer
  mInstruction <- asks (V.!? currentPointer)
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

executeInstruction :: Instruction -> VirtualMachine ()
executeInstruction ProgramEnd = throwError "Program end should be unreachable by execution loop"

virtualMachine :: VirtualMachine ()
virtualMachine = whileM_ executionCondition executionAction

addressConversion :: TypeBounds -> Address -> Either String Int
addressConversion (TypeBounds vLower vUpper tLower tUpper) address
  | address >= vLower && address <= vUpper = Right $ address - vLower
  | address >= tLower && address <= tUpper = Right $ address - tLower + vUpper + 1
  | otherwise = Left "Address is not convertable"

getLocalContext :: VirtualMachine LocalContext
getLocalContext = do
  contexts <- gets localContexts
  case contexts of
    context : _ -> return context
    []          -> throwError "Not inside a local context"

addressContextType :: Address -> ContextType
addressContextType address
  | address >= 1 && address <= 12000 = Global
  | address >= 12001 && address <= 44000 = Local
  | otherwise = Static

getValueFromIndex :: MachineType -> MemoryBlock -> Int -> Maybe TypeWrapper
getValueFromIndex IntType MemoryBlock{..} index = IntWrapper <$> intMemory V.!? index
getValueFromIndex FloatType MemoryBlock{..} index = FloatWrapper <$> floatMemory V.!? index
getValueFromIndex CharType MemoryBlock{..} index = CharWrapper <$> charMemory V.!? index
getValueFromIndex BoolType MemoryBlock{..} index = BoolWrapper <$> boolMemory V.!? index

updateMemoryBlock :: Int -> TypeWrapper -> MemoryBlock -> MemoryBlock
updateMemoryBlock index (IntWrapper int) mBlock@MemoryBlock{intMemory=memory} = mBlock{intMemory = memory V.// [(index,int)]}
updateMemoryBlock index (FloatWrapper float) mBlock@MemoryBlock{floatMemory=memory} = mBlock{floatMemory = memory V.// [(index, float)]}
updateMemoryBlock index (CharWrapper char) mBlock@MemoryBlock{charMemory=memory} = mBlock{charMemory = memory V.// [(index, char)]}
updateMemoryBlock index (BoolWrapper bool) mBlock@MemoryBlock{boolMemory=memory} = mBlock{boolMemory = memory V.// [(index, bool)]}

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

getValueFromAddress :: Address -> VirtualMachine TypeWrapper
getValueFromAddress address = do
  LocalContext{localMemory=memory, addressType=addressConvertor} <- addressContext address
  let (addressType, bounds) = addressConvertor address
  index <- liftEither $ addressConversion bounds address
  liftEither $ maybe (Left "Address out of type bounds") Right $ getValueFromIndex addressType memory index

setValue :: TypeWrapper -> Address -> VirtualMachine ()
setValue value address = do
  let contextType = addressContextType address
  localContext@LocalContext{localMemory=memory, addressType=addressConvertor} <- addressContext address
  let (_, bounds) = addressConvertor address
  index <- liftEither $ addressConversion bounds address
  let nMemory = updateMemoryBlock index value memory
  modify $ updateContext contextType localContext{localMemory=nMemory}
