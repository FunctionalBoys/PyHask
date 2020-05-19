module VirtualMachine.VMExecution (virtualMachine) where

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
executionCondition = (/= ProgramEnd) <$> nextInstruction

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
