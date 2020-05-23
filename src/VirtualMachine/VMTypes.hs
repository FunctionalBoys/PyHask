module VirtualMachine.VMTypes where

import           Control.Monad.Except     (ExceptT)
import           Control.Monad.Reader     (ReaderT)
import           Control.Monad.State.Lazy (StateT)
import           Data.Default.Class
import           Data.Vector              (Vector)

type Address = Int
type Pointer = Int
type Operation = TypeWrapper -> TypeWrapper -> Maybe TypeWrapper
data Instruction =
    ProgramEnd deriving (Eq,Show)

data MachineType = IntType | BoolType | CharType | FloatType deriving (Eq,Show)

data TypeWrapper = IntWrapper Int | FloatWrapper Double | CharWrapper Char | BoolWrapper Bool deriving (Eq,Show)

data ContextType = Local | Global | Static deriving (Eq,Show)

data LocalContext = LocalContext { checkpoint :: Pointer,
                                   localMemory :: MemoryBlock,
                                   addressType :: Address -> (MachineType, TypeBounds)
                                 }

data MachineState = MachineState { instructionPointer :: Pointer,
                                   globalContext      :: LocalContext,
                                   staticContext      :: LocalContext,
                                   localContexts      :: [LocalContext]
                                 }

data TypeBounds = TypeBounds { varLower  :: Int,
                               varUpper  :: Int,
                               tempLower :: Int,
                               tempUpper :: Int
                             } deriving (Eq,Show)

data MemoryBounds = MemoryBounds { intBounds   :: TypeBounds,
                                   floatBounds :: TypeBounds,
                                   charBounds  :: TypeBounds,
                                   boolBounds  :: TypeBounds
                                 } deriving (Eq,Show)

type VirtualMachine = ReaderT (Vector Instruction) (StateT MachineState (ExceptT String IO))

isInBounds :: TypeBounds -> Address -> Bool
isInBounds (TypeBounds vLower vUpper tLower tUpper) address = address >= vLower && address <= vUpper || address >= tLower && address <= tUpper

addressInfo :: MemoryBounds -> Address -> (MachineType, TypeBounds)
addressInfo (MemoryBounds ints floats chars bools) address
  | isInBounds ints address = (IntType, ints)
  | isInBounds floats address = (FloatType, floats)
  | isInBounds chars address = (CharType, chars)
  | otherwise = (BoolType, bools)
