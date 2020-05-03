module GenUtils where

import           Control.Category
import           Control.Monad.State.Lazy
import qualified Data.Sequence            as S
import           ParserTypes
import           Utils

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
