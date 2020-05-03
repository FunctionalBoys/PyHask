module GenUtils where

import           Control.Category
import           Control.Monad.State.Lazy
import qualified Data.List.NonEmpty       as N
import qualified Data.Sequence            as S
import           ParserTypes
import           Utils
import qualified Data.HashMap.Strict      as H

quadruplesCounter :: ParserState -> Int
quadruplesCounter (ParserState _ _ _ quads _) = S.length quads

addQuadruple :: Quad -> ParserState -> ParserState
addQuadruple quad (ParserState ss fs cs quads literals) = ParserState ss fs cs (quads S.|> quad) literals

updateQuadruple :: Int -> Quad -> ParserState -> ParserState
updateQuadruple index quad (ParserState ss fs cs quads literals) = ParserState ss fs cs (S.update index quad quads) literals

lookupQuadruple :: Int -> ParserState -> Maybe Quad
lookupQuadruple i (ParserState _ _ _ quads _) = quads S.!? i

safeQuadrupleUpdate :: (Quad -> Either String Quad) -> Int -> Parser ()
safeQuadrupleUpdate f index = do
  mQuad <- gets $ lookupQuadruple index
  quad <- maybeFail ("No quadruple at index " ++ show index) mQuad
  newQuad <- liftEither <<< f $ quad
  modify $ updateQuadruple index newQuad

memoryBlockToMaybeAddress :: TypeMemoryBlock -> Maybe Address
memoryBlockToMaybeAddress (TypeMemoryBlock _ mUBound cDirection )
  | cDirection <= mUBound = Just (Address cDirection)
  | otherwise = Nothing

getNextAddress :: SimpleType -> MemoryBlock -> Maybe Address
getNextAddress IntType (MemoryBlock typeMemoryBlock _ _ _) = memoryBlockToMaybeAddress typeMemoryBlock
getNextAddress FloatType (MemoryBlock _ typeMemoryBlock _ _) = memoryBlockToMaybeAddress typeMemoryBlock
getNextAddress CharType (MemoryBlock _ _ typeMemoryBlock _) = memoryBlockToMaybeAddress typeMemoryBlock
getNextAddress BoolType (MemoryBlock _ _ _ typeMemoryBlock) = memoryBlockToMaybeAddress typeMemoryBlock

getNextVariableAddress :: SimpleType -> ParserState -> Maybe Address
getNextVariableAddress sType (ParserState (Scope _ _ _ memoryblock _ N.:| _) _ _ _ _) = getNextAddress sType memoryblock

memoryBlockIncrease :: TypeMemoryBlock -> TypeMemoryBlock
memoryBlockIncrease (TypeMemoryBlock mLBound mUBound cDirection) = TypeMemoryBlock mLBound mUBound (cDirection + 1)

increaseCurrentAddress :: SimpleType -> MemoryBlock -> MemoryBlock
increaseCurrentAddress IntType (MemoryBlock tMBI tMF tMC tMB) = MemoryBlock (memoryBlockIncrease tMBI) tMF tMC tMB
increaseCurrentAddress FloatType (MemoryBlock tMBI tMF tMC tMB) = MemoryBlock tMBI (memoryBlockIncrease tMF) tMC tMB
increaseCurrentAddress CharType (MemoryBlock tMBI tMF tMC tMB) = MemoryBlock tMBI tMF (memoryBlockIncrease tMC) tMB
increaseCurrentAddress BoolType (MemoryBlock tMBI tMF tMC tMB) = MemoryBlock tMBI tMF tMC (memoryBlockIncrease tMB)

getNextTypeAddress :: SimpleType -> MemoryBlock -> Parser (MemoryBlock, Address)
getNextTypeAddress sT mB = do
  let mAddress = getNextAddress sT mB
  address <- maybeFail "Out of memory error" mAddress
  return (increaseCurrentAddress sT mB, address)

currentMemoryBlock :: ParserState -> MemoryBlock
currentMemoryBlock (ParserState (Scope _ _ _ memoryblock _ N.: _) _ _ _ _) = memoryblock

updateCurrentMemoryBlock :: MemoryBlock -> ParserState -> ParserState
updateCurrentMemoryBlock memoryBlock (ParserState (Scope sT sI sV _ sTM N.: next) q w e r) = (ParserState (Scope sT sI sV memoryBlock sTM N.: next) q w e r)

updateCurrentTemp :: MemoryBlock -> ParserState -> ParserState
updateCurrentTemp memoryBlock (ParserState (Scope sT sI sV sM _ N.: next) q w e r) = (ParserState (Scope sT sI sV SM memoryBlock N.: next) q w e r)

lookupLiteral :: Literal -> ParserState -> Maybe Address
lookupLiteral literal (ParserState _ _ _ _ (LiteralBlock _ lMap)) = H.lookup literal lMap