{-# LANGUAGE RecordWildCards #-}

module VirtualMachine.ConversionUtils where

import qualified Data.Vector            as V
import           VirtualMachine.VMTypes

updateMemoryBlock :: Int -> TypeWrapper -> MemoryBlock -> MemoryBlock
updateMemoryBlock index (IntWrapper int) mBlock@MemoryBlock{intMemory=memory} = mBlock{intMemory = memory V.// [(index,int)]}
updateMemoryBlock index (FloatWrapper float) mBlock@MemoryBlock{floatMemory=memory} = mBlock{floatMemory = memory V.// [(index, float)]}
updateMemoryBlock index (CharWrapper char) mBlock@MemoryBlock{charMemory=memory} = mBlock{charMemory = memory V.// [(index, char)]}
updateMemoryBlock index (BoolWrapper bool) mBlock@MemoryBlock{boolMemory=memory} = mBlock{boolMemory = memory V.// [(index, bool)]}

getValueFromIndex :: MachineType -> MemoryBlock -> Int -> Maybe TypeWrapper
getValueFromIndex IntType MemoryBlock{..} index = IntWrapper <$> intMemory V.!? index
getValueFromIndex FloatType MemoryBlock{..} index = FloatWrapper <$> floatMemory V.!? index
getValueFromIndex CharType MemoryBlock{..} index = CharWrapper <$> charMemory V.!? index
getValueFromIndex BoolType MemoryBlock{..} index = BoolWrapper <$> boolMemory V.!? index

addressConversion :: TypeBounds -> Address -> Either String Int
addressConversion (TypeBounds vLower vUpper tLower tUpper) address
  | address >= vLower && address <= vUpper = Right $ address - vLower
  | address >= tLower && address <= tUpper = Right $ address - tLower + (vUpper - vLower + 1)
  | otherwise = Left "Address is not convertable"
