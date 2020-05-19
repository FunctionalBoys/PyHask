module VirtualMachine.VMTypes where

import           Control.Monad.Except     (ExceptT)
import           Control.Monad.Reader     (ReaderT)
import           Control.Monad.State.Lazy (StateT)
import           Data.Default.Class
import           Data.Vector              (Vector)

data Instruction =
    ProgramEnd deriving (Eq,Show)

data MachineState = MachineState { instructionPointer :: Int
                                 } deriving (Eq,Show)

type VirtualMachine = ReaderT (Vector Instruction) (StateT MachineState (ExceptT String IO))

instance Default MachineState where
  def = MachineState 0
