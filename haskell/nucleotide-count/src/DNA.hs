module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map.Map Nucleotide Int)
nucleotideCounts nucs = foldNuc nucs (Right nucleotideMap)

foldNuc :: String -> Either String (Map.Map Nucleotide Int) -> Either String (Map.Map Nucleotide Int)
foldNuc _ (Left x) = Left x
foldNuc [] map = map
foldNuc (nuc:nucs) (Right map) = testNuc nuc map >>= \x -> foldNuc nucs (Right x)

testNuc :: Char -> Map.Map Nucleotide Int -> Either String (Map.Map Nucleotide Int)
testNuc 'A' map = Right $ Map.alter (\(Just x) -> Just (x+1)) A map
testNuc 'T' map = Right $ Map.alter (\(Just x) -> Just (x+1)) T map
testNuc 'C' map = Right $ Map.alter (\(Just x) -> Just (x+1)) C map
testNuc 'G' map = Right $ Map.alter (\(Just x) -> Just (x+1)) G map
testNuc  _  _ = Left "Error"

nucleotideMap :: Map.Map Nucleotide Int
nucleotideMap =  Map.fromList [(A,0),(C,0),(G,0),(T,0)]
