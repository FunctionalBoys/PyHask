{-# LANGUAGE RecordWildCards #-}

module VirtualMachine.StateInitializer (createVirtualMachineState, createMemBlock) where

import           Control.Monad.State.Lazy
import qualified Data.Vector                    as V
import           VirtualMachine.ConversionUtils
import           VirtualMachine.VMTypes
import qualified Data.Sequence as S

vectorSize :: TypeBounds -> Int
vectorSize (TypeBounds vLower vUpper tLower tUpper) = (vUpper - vLower + 1) + (tUpper - tLower + 1)

addToMemoryState :: (Address -> TypeBounds) -> (TypeWrapper, Address) -> StateT MemoryBlock (Either String) ()
addToMemoryState f tuple = do
  block <- get
  updatedBlock <- lift $ addToMemory f tuple block
  put updatedBlock

addToMemory  :: (Address -> TypeBounds) -> (TypeWrapper, Address) -> MemoryBlock -> Either String MemoryBlock
addToMemory f (value, address) block = do
  index <- addressConversion (f address) address
  return $ updateMemoryBlock index value block

createMemBlock :: MemoryBounds -> MemoryBlock
createMemBlock MemoryBounds{..} = MemoryBlock (V.replicate (vectorSize charBounds) 'a') (V.replicate (vectorSize intBounds) 0) (V.replicate (vectorSize floatBounds) 0.0) (V.replicate (vectorSize boolBounds) True)

createVirtualMachineState :: ParserResult -> Either String MachineState
createVirtualMachineState ParserResult{globalBounds=gBounds, staticBounds=sBounds, parsedLiterals=literals} = do
  let infoFunction = addressInfo sBounds
  let staticMemBlock = createMemBlock sBounds
  filledStaticBlock <- execStateT (forM_ literals (addToMemoryState (snd . infoFunction))) staticMemBlock
  return $ MachineState 0 (LocalContext 0 (createMemBlock gBounds) (addressInfo gBounds)) (LocalContext 0 filledStaticBlock infoFunction) [] NoContext S.empty 0
