module GenUtils where

import           AnalysisUtils
import           Control.Category
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as H
import qualified Data.List.NonEmpty       as N
import           Data.Maybe
import qualified Data.Sequence            as S
import           ParserTypes
import           Utils

quadruplesCounter :: ParserState -> Int
quadruplesCounter ParserState{quadruplesSequence=quads} = S.length quads

addQuadruple :: Quad -> ParserState -> ParserState
addQuadruple quad pState@ParserState{quadruplesSequence=quads}= pState{ quadruplesSequence = quads S.|> quad}

registerQuadruple :: Quad -> Parser ()
registerQuadruple = modify <<< addQuadruple

updateQuadruple :: Int -> Quad -> ParserState -> ParserState
updateQuadruple index quad pState@ParserState{quadruplesSequence=quads} = pState {quadruplesSequence= S.update index quad quads}

lookupQuadruple :: Int -> ParserState -> Maybe Quad
lookupQuadruple i ParserState{quadruplesSequence=quads} = quads S.!? i

safeQuadrupleUpdate :: (Quad -> Either String Quad) -> Int -> Parser ()
safeQuadrupleUpdate f index = do
  mQuad <- gets $ lookupQuadruple index
  quad <- maybeFail ("No quadruple at index " ++ show index) mQuad
  newQuad <- liftEither <<< f $ quad
  modify $ updateQuadruple index newQuad

fillGOTOF :: Int -> Quad -> Either String Quad
fillGOTOF index (QuadFPlaceholder address) = Right $ QuadF address index
fillGOTOF _ quad = Left $ show quad ++ " is not goto false placeholder"

fillGOTOT :: Int -> Quad -> Either String Quad
fillGOTOT index (QuadTPlaceholder address) = Right $ QuadT address index
fillGOTOT _ quad = Left $ show quad ++ " is not goto true placeholder"

fillGOTO :: Int -> Quad -> Either String Quad
fillGOTO index QuadGOTOPlaceholder = Right $ QuadGOTO index
fillGOTO _  quad = Left $ show quad ++ " is not goto placeholder"

memoryBlockToMaybeAddress :: TypeMemoryBlock -> Maybe Address
memoryBlockToMaybeAddress (TypeMemoryBlock _ mUBound cDirection )
  | cDirection <= mUBound = Just (Address cDirection)
  | otherwise = Nothing

getNextAddress :: SimpleType -> MemoryBlock -> Maybe Address
getNextAddress IntType   = memoryBlockToMaybeAddress <<< memoryBlockInt
getNextAddress FloatType = memoryBlockToMaybeAddress <<< memoryBlockFloat
getNextAddress CharType  = memoryBlockToMaybeAddress <<< memoryBlockChar
getNextAddress BoolType  = memoryBlockToMaybeAddress <<< memoryBlockBool

getScopeTempMemoryBlock :: ParserState -> MemoryBlock
getScopeTempMemoryBlock ParserState{scopes=Scope{scopeTempMemory=memory} N.:| _} = memory

updateTypeMemory :: TypeMemoryBlock -> TypeMemoryBlock -> Either String TypeMemoryBlock
updateTypeMemory (TypeMemoryBlock lower upper current) (TypeMemoryBlock newLower newUpper newCurrent)
  | lower == newLower && upper == newUpper = Right (TypeMemoryBlock lower upper (max current newCurrent))
  | otherwise = Left "Type memory blocks do not share size"

updateMemoryBlock :: MemoryBlock -> MemoryBlock -> Either String MemoryBlock
updateMemoryBlock (MemoryBlock mbi mbf mbc mbb) (MemoryBlock nmbi nmbf nmbc nmbb) = MemoryBlock
  <$> updateTypeMemory mbi nmbi
  <*> updateTypeMemory mbf nmbf
  <*> updateTypeMemory mbc nmbc
  <*> updateTypeMemory mbb nmbb

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
currentMemoryBlock ParserState{scopes=(Scope{scopeVariablesMemory=memoryBlock} N.:| _)} = memoryBlock

currentTempBlock :: ParserState -> MemoryBlock
currentTempBlock ParserState{scopes=(Scope{scopeTempMemory=memoryBlock} N.:| _)} = memoryBlock

updateCurrentMemoryBlock :: MemoryBlock -> ParserState -> ParserState
updateCurrentMemoryBlock memoryBlock pState@ParserState{scopes=(currentScope@Scope{} N.:| restScopes)} = pState {scopes=currentScope{scopeVariablesMemory=memoryBlock} N.:| restScopes}

updateCurrentTemp :: MemoryBlock -> ParserState -> ParserState
updateCurrentTemp memoryBlock pState@ParserState{scopes=(currentScope@Scope{} N.:| restScopes)} = pState {scopes=currentScope{scopeTempMemory=memoryBlock} N.:| restScopes}

nextAddress :: (ParserState -> MemoryBlock) -> (MemoryBlock -> ParserState -> ParserState) -> Maybe (FunctionDefinition -> MemoryBlock , MemoryBlock -> FunctionDefinition -> FunctionDefinition) -> SimpleType -> Parser Address
nextAddress fetch push sideEffects sType = do
  mB <- gets fetch
  (nMB, address) <- getNextTypeAddress sType mB
  isInsideFunction <- insideFunction
  when (isInsideFunction && isJust sideEffects) $ do
      (memBlock, memUpdate) <- maybeFail "Tried to update function without giving how" sideEffects
      fName <- findScopeFunctionName
      functionDefinition <- findFunction fName
      let oldMemoryBlock = memBlock functionDefinition
      maxMB <- liftEither $ updateMemoryBlock oldMemoryBlock nMB
      let newFDef = memUpdate maxMB functionDefinition
      modify $ insertFunction fName newFDef
  address <$ (modify <<< push) nMB

nextVarAddress :: SimpleType -> Parser Address
nextVarAddress = nextAddress currentMemoryBlock updateCurrentMemoryBlock $ Just (functionDefinitionVarMB, updateDef)
  where
    updateDef mB fDef = fDef{functionDefinitionVarMB = mB}

nextTempAddress :: SimpleType -> Parser Address
nextTempAddress = nextAddress currentTempBlock updateCurrentTemp $ Just (functionDefinitionTempMB, updateDef)
  where
    updateDef mB fDef = fDef{functionDefinitionTempMB = mB}

lookupLiteral :: Literal -> ParserState -> Maybe Address
lookupLiteral literal ParserState{literalBlock=LiteralBlock{literalAddressMap=lMap}} = H.lookup literal lMap

getLiteralMemoryBlock :: ParserState -> MemoryBlock
getLiteralMemoryBlock ParserState{literalBlock=LiteralBlock{literalMemoryBlock=mBlock}} = mBlock

updateLiteralMemoryBlock :: MemoryBlock -> ParserState -> ParserState
updateLiteralMemoryBlock memoryBlock pState@ParserState{literalBlock=lBlock@LiteralBlock{}} = pState{literalBlock=lBlock{literalMemoryBlock=memoryBlock}}

nextLiteralAddress :: SimpleType -> Parser Address
nextLiteralAddress = nextAddress getLiteralMemoryBlock updateLiteralMemoryBlock Nothing

insertLiteralAddress :: Literal -> Address -> ParserState -> ParserState
insertLiteralAddress literal address pState@ParserState{literalBlock=lBlock@LiteralBlock{literalAddressMap=lMap}} = pState{literalBlock=lBlock{literalAddressMap=H.insert literal address lMap}}

literalType :: Literal -> SimpleType
literalType (LiteralInt _)   = IntType
literalType (LiteralFloat _) = FloatType
literalType (LiteralChar _)  = CharType
literalType (LiteralBool _)  = BoolType

getLiteralAddress :: Literal -> Parser Address
getLiteralAddress literal = do
  mAddress <- gets $ lookupLiteral literal
  case mAddress of
    Just address -> return address
    Nothing -> do
      let lType = literalType literal
      address <- nextLiteralAddress lType
      modify $ insertLiteralAddress literal address
      return address
