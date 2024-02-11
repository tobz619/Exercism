{-# LANGUAGE GADTs #-}
module Alphametics (solve) where

import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void(Void)
import qualified Data.Map as Map
import Data.List ( delete, nub, transpose )
import Data.Char ( toUpper )
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes)
import Data.List.NonEmpty (nonEmpty, toList, NonEmpty ((:|)))
import Control.Monad.State (get, put, modify')
import Control.Applicative (liftA2)
import Data.Function (on)
import Debug.Trace (traceShowId, traceShow)


type PossibleChars = Map.Map Char [Int]

data Oper a where
    Val :: Oper a
    Add :: String -> String -> Oper String
    Sub :: String -> String -> Oper String

type Parser = Parsec Void String

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."

addP :: Parser (Oper String)
addP = do a <- map toUpper <$> many letterChar
          _ <- space1
          _ <- char '+'
          _ <- space1
          b <- map toUpper <$> many letterChar
          return (Add a b)



updateMap ::  PossibleChars -> [(Char, Int)] -> PossibleChars
updateMap = foldr (uncurry setChar)
        where setChar ch v = Map.mapWithKey (selector ch)
               where selector toChange key xs
                        | key == toChange = [v]
                        | key == ' ' = [0]
                        | otherwise = delete v xs

addChars :: String -> Char -> Int -> Map.Map Char [Int] -> Maybe [(Int, [(Char, Int)], (Char, Int))]
addChars cs resChar carry charMap = do
            as <- mapM (`Map.lookup` charMap) cs
            resVals <- resChar `Map.lookup` charMap
            let valCombi = sequence as
            return [(c, zip cs v1 ,(resChar, resDig)) |
                    v1 <- valCombi,
                    let (c, resDig) = (`divMod` 10) . (carry +) $ sum v1,
                    resDig `elem` resVals,
                    (sum v1 `mod` 10) == resDig,
                    allUnique v1,
                    resDig `notElem` v1
                    ]

-- >>> processFun addChars "DE" 'Y' 0 (initChars "DEY")
-- Just [(0,fromList [(' ',[0]),('D',[1]),('E',[2]),('Y',[3])]),(0,fromList [(' ',[0]),('D',[1]),('E',[3]),('Y',[4])]),(0,fromList [(' ',[0]),('D',[1]),('E',[4]),('Y',[5])]),(0,fromList [(' ',[0]),('D',[1]),('E',[5]),('Y',[6])]),(0,fromList [(' ',[0]),('D',[1]),('E',[6]),('Y',[7])]),(0,fromList [(' ',[0]),('D',[1]),('E',[7]),('Y',[8])]),(0,fromList [(' ',[0]),('D',[1]),('E',[8]),('Y',[9])]),(1,fromList [(' ',[0]),('D',[1]),('E',[9]),('Y',[0])]),(0,fromList [(' ',[0]),('D',[2]),('E',[1]),('Y',[3])]),(0,fromList [(' ',[0]),('D',[2]),('E',[3]),('Y',[5])]),(0,fromList [(' ',[0]),('D',[2]),('E',[4]),('Y',[6])]),(0,fromList [(' ',[0]),('D',[2]),('E',[5]),('Y',[7])]),(0,fromList [(' ',[0]),('D',[2]),('E',[6]),('Y',[8])]),(0,fromList [(' ',[0]),('D',[2]),('E',[7]),('Y',[9])]),(1,fromList [(' ',[0]),('D',[2]),('E',[8]),('Y',[0])]),(1,fromList [(' ',[0]),('D',[2]),('E',[9]),('Y',[1])]),(0,fromList [(' ',[0]),('D',[3]),('E',[1]),('Y',[4])]),(0,fromList [(' ',[0]),('D',[3]),('E',[2]),('Y',[5])]),(0,fromList [(' ',[0]),('D',[3]),('E',[4]),('Y',[7])]),(0,fromList [(' ',[0]),('D',[3]),('E',[5]),('Y',[8])]),(0,fromList [(' ',[0]),('D',[3]),('E',[6]),('Y',[9])]),(1,fromList [(' ',[0]),('D',[3]),('E',[7]),('Y',[0])]),(1,fromList [(' ',[0]),('D',[3]),('E',[8]),('Y',[1])]),(1,fromList [(' ',[0]),('D',[3]),('E',[9]),('Y',[2])]),(0,fromList [(' ',[0]),('D',[4]),('E',[1]),('Y',[5])]),(0,fromList [(' ',[0]),('D',[4]),('E',[2]),('Y',[6])]),(0,fromList [(' ',[0]),('D',[4]),('E',[3]),('Y',[7])]),(0,fromList [(' ',[0]),('D',[4]),('E',[5]),('Y',[9])]),(1,fromList [(' ',[0]),('D',[4]),('E',[6]),('Y',[0])]),(1,fromList [(' ',[0]),('D',[4]),('E',[7]),('Y',[1])]),(1,fromList [(' ',[0]),('D',[4]),('E',[8]),('Y',[2])]),(1,fromList [(' ',[0]),('D',[4]),('E',[9]),('Y',[3])]),(0,fromList [(' ',[0]),('D',[5]),('E',[1]),('Y',[6])]),(0,fromList [(' ',[0]),('D',[5]),('E',[2]),('Y',[7])]),(0,fromList [(' ',[0]),('D',[5]),('E',[3]),('Y',[8])]),(0,fromList [(' ',[0]),('D',[5]),('E',[4]),('Y',[9])]),(1,fromList [(' ',[0]),('D',[5]),('E',[6]),('Y',[1])]),(1,fromList [(' ',[0]),('D',[5]),('E',[7]),('Y',[2])]),(1,fromList [(' ',[0]),('D',[5]),('E',[8]),('Y',[3])]),(1,fromList [(' ',[0]),('D',[5]),('E',[9]),('Y',[4])]),(0,fromList [(' ',[0]),('D',[6]),('E',[1]),('Y',[7])]),(0,fromList [(' ',[0]),('D',[6]),('E',[2]),('Y',[8])]),(0,fromList [(' ',[0]),('D',[6]),('E',[3]),('Y',[9])]),(1,fromList [(' ',[0]),('D',[6]),('E',[4]),('Y',[0])]),(1,fromList [(' ',[0]),('D',[6]),('E',[5]),('Y',[1])]),(1,fromList [(' ',[0]),('D',[6]),('E',[7]),('Y',[3])]),(1,fromList [(' ',[0]),('D',[6]),('E',[8]),('Y',[4])]),(1,fromList [(' ',[0]),('D',[6]),('E',[9]),('Y',[5])]),(0,fromList [(' ',[0]),('D',[7]),('E',[1]),('Y',[8])]),(0,fromList [(' ',[0]),('D',[7]),('E',[2]),('Y',[9])]),(1,fromList [(' ',[0]),('D',[7]),('E',[3]),('Y',[0])]),(1,fromList [(' ',[0]),('D',[7]),('E',[4]),('Y',[1])]),(1,fromList [(' ',[0]),('D',[7]),('E',[5]),('Y',[2])]),(1,fromList [(' ',[0]),('D',[7]),('E',[6]),('Y',[3])]),(1,fromList [(' ',[0]),('D',[7]),('E',[8]),('Y',[5])]),(1,fromList [(' ',[0]),('D',[7]),('E',[9]),('Y',[6])]),(0,fromList [(' ',[0]),('D',[8]),('E',[1]),('Y',[9])]),(1,fromList [(' ',[0]),('D',[8]),('E',[2]),('Y',[0])]),(1,fromList [(' ',[0]),('D',[8]),('E',[3]),('Y',[1])]),(1,fromList [(' ',[0]),('D',[8]),('E',[4]),('Y',[2])]),(1,fromList [(' ',[0]),('D',[8]),('E',[5]),('Y',[3])]),(1,fromList [(' ',[0]),('D',[8]),('E',[6]),('Y',[4])]),(1,fromList [(' ',[0]),('D',[8]),('E',[7]),('Y',[5])]),(1,fromList [(' ',[0]),('D',[8]),('E',[9]),('Y',[7])]),(1,fromList [(' ',[0]),('D',[9]),('E',[1]),('Y',[0])]),(1,fromList [(' ',[0]),('D',[9]),('E',[2]),('Y',[1])]),(1,fromList [(' ',[0]),('D',[9]),('E',[3]),('Y',[2])]),(1,fromList [(' ',[0]),('D',[9]),('E',[4]),('Y',[3])]),(1,fromList [(' ',[0]),('D',[9]),('E',[5]),('Y',[4])]),(1,fromList [(' ',[0]),('D',[9]),('E',[6]),('Y',[5])]),(1,fromList [(' ',[0]),('D',[9]),('E',[7]),('Y',[6])]),(1,fromList [(' ',[0]),('D',[9]),('E',[8]),('Y',[7])])]
processFun :: (String -> Char  -> Int  -> PossibleChars  -> Maybe [(Int, [(Char, Int)], (Char, Int))]) 
              -> String -> Char -> Int -> PossibleChars -> [(Int, PossibleChars)]
processFun f str ch car charMap =
    let results = f str ch car charMap
        mkPairs = map (\ ~(c, ps, r) -> (c,r:ps)) <$> results
        newMaps = fmap (\ ~(c, pcs) -> (c, updateMap charMap pcs)) <$> mkPairs
     in concat newMaps

cycleFunc = go 0
    where 
          go carry f (s:strs) (c:chrs) mp = let validMaps = processFun f s c carry mp
                                             in do (ca, newMap)  <- validMaps
                                                   go ca f strs chrs (traceShowId newMap)

standardiseInps :: [String] -> String -> ([String], String)
standardiseInps inps res = (transpose lined, normaledRes)
    where (normaledRes: lined) = map (reverse . addPadding padAmount) (res:inps)
          padAmount = maximum . map length $ (res:inps)
          addPadding amount x = replicate (amount - length x) ' ' ++ x

-- >>> pairColumns ["SEND","MORE"] "MONEY"
-- (["DE","NR","EO","SM","  "],"YENOM")
pairColumns :: [String] -> String -> ([String], [Char])
pairColumns inps = unzip . uncurry zip . standardiseInps inps

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs


initChars :: [Char] -> Map.Map Char [Int]
initChars xs = Map.insert ' ' [0] $ foldr initiator Map.empty (nub xs :: String)
        where initiator x = Map.insert x [0..9]

-- >>> addChars "DE" 'Y' 0 (initChars "YDE")
-- Just [(0,[('D',1),('E',2)],('Y',3)),(0,[('D',1),('E',3)],('Y',4)),(0,[('D',1),('E',4)],('Y',5)),(0,[('D',1),('E',5)],('Y',6)),(0,[('D',1),('E',6)],('Y',7)),(0,[('D',1),('E',7)],('Y',8)),(0,[('D',1),('E',8)],('Y',9)),(1,[('D',1),('E',9)],('Y',0)),(0,[('D',2),('E',1)],('Y',3)),(0,[('D',2),('E',3)],('Y',5)),(0,[('D',2),('E',4)],('Y',6)),(0,[('D',2),('E',5)],('Y',7)),(0,[('D',2),('E',6)],('Y',8)),(0,[('D',2),('E',7)],('Y',9)),(1,[('D',2),('E',8)],('Y',0)),(1,[('D',2),('E',9)],('Y',1)),(0,[('D',3),('E',1)],('Y',4)),(0,[('D',3),('E',2)],('Y',5)),(0,[('D',3),('E',4)],('Y',7)),(0,[('D',3),('E',5)],('Y',8)),(0,[('D',3),('E',6)],('Y',9)),(1,[('D',3),('E',7)],('Y',0)),(1,[('D',3),('E',8)],('Y',1)),(1,[('D',3),('E',9)],('Y',2)),(0,[('D',4),('E',1)],('Y',5)),(0,[('D',4),('E',2)],('Y',6)),(0,[('D',4),('E',3)],('Y',7)),(0,[('D',4),('E',5)],('Y',9)),(1,[('D',4),('E',6)],('Y',0)),(1,[('D',4),('E',7)],('Y',1)),(1,[('D',4),('E',8)],('Y',2)),(1,[('D',4),('E',9)],('Y',3)),(0,[('D',5),('E',1)],('Y',6)),(0,[('D',5),('E',2)],('Y',7)),(0,[('D',5),('E',3)],('Y',8)),(0,[('D',5),('E',4)],('Y',9)),(1,[('D',5),('E',6)],('Y',1)),(1,[('D',5),('E',7)],('Y',2)),(1,[('D',5),('E',8)],('Y',3)),(1,[('D',5),('E',9)],('Y',4)),(0,[('D',6),('E',1)],('Y',7)),(0,[('D',6),('E',2)],('Y',8)),(0,[('D',6),('E',3)],('Y',9)),(1,[('D',6),('E',4)],('Y',0)),(1,[('D',6),('E',5)],('Y',1)),(1,[('D',6),('E',7)],('Y',3)),(1,[('D',6),('E',8)],('Y',4)),(1,[('D',6),('E',9)],('Y',5)),(0,[('D',7),('E',1)],('Y',8)),(0,[('D',7),('E',2)],('Y',9)),(1,[('D',7),('E',3)],('Y',0)),(1,[('D',7),('E',4)],('Y',1)),(1,[('D',7),('E',5)],('Y',2)),(1,[('D',7),('E',6)],('Y',3)),(1,[('D',7),('E',8)],('Y',5)),(1,[('D',7),('E',9)],('Y',6)),(0,[('D',8),('E',1)],('Y',9)),(1,[('D',8),('E',2)],('Y',0)),(1,[('D',8),('E',3)],('Y',1)),(1,[('D',8),('E',4)],('Y',2)),(1,[('D',8),('E',5)],('Y',3)),(1,[('D',8),('E',6)],('Y',4)),(1,[('D',8),('E',7)],('Y',5)),(1,[('D',8),('E',9)],('Y',7)),(1,[('D',9),('E',1)],('Y',0)),(1,[('D',9),('E',2)],('Y',1)),(1,[('D',9),('E',3)],('Y',2)),(1,[('D',9),('E',4)],('Y',3)),(1,[('D',9),('E',5)],('Y',4)),(1,[('D',9),('E',6)],('Y',5)),(1,[('D',9),('E',7)],('Y',6)),(1,[('D',9),('E',8)],('Y',7))]

-- >>> testInp 
-- []
testInp = cycleFunc addChars cols chrs mp
    where mp = initChars "SENDMOREMONEY"
          (cols, chrs) = pairColumns ["SEND","MORE"] "MONEY"
