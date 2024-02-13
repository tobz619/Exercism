{-# LANGUAGE BangPatterns #-}
module Alphametics (solve) where

import qualified Data.Map as Map
import Data.List ( delete, nub, transpose, foldl')
import Data.Char ( toUpper )
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes)
import Control.Applicative (liftA2)
import Debug.Trace (traceShowId, traceShow)


type PossibleChars = Map.Map Char [Int]

data Operator = Plus | Mult deriving Show
data Expr = Value String
          | Expr Operator Expr Expr
          deriving Show

data Equation = Equation { eExpr :: Expr
                         , value :: String } deriving Show

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."


updateMap ::  PossibleChars -> [(Char, Int)] -> PossibleChars
updateMap = foldr (uncurry setChar)
        where setChar ch v = Map.mapWithKey (selector ch)
                where selector ' ' _ xs = xs
                      selector _ ' ' xs = xs
                      selector toChange key xs
                        | key == toChange = [v]
                        | otherwise = delete v xs


checkNull :: (Int, PossibleChars) -> Maybe (Int, PossibleChars)
checkNull (c,mp) = (,) c <$> traverse (\x -> if null x then Nothing else Just x) mp


addChars :: String -> Char -> Int -> Map.Map Char [Int] -> Maybe [(Int, PossibleChars)]
addChars cs resChar carry charMap = do
    as <- traverse (`Map.lookup` charMap) cs
    resVals <- resChar `Map.lookup` charMap
    let valCombi = sequence as

    return [ ( c, toReturn ) |
        v1 <- valCombi,
        let (c, resDig) = (`divMod` 10) . (carry +) $ sum v1,
        resDig `elem` resVals,
        let !toReturn = updateMap charMap ((resChar,resDig): zip cs v1)
        ]


processFun :: (String  -> Char -> Int -> PossibleChars -> Maybe [(Int, PossibleChars)])
            -> String -> Char -> Int -> PossibleChars -> [(Int, PossibleChars)]
processFun f str ch car charMap = let
        results = fromMaybe [] $ f str ch car charMap
     in mapMaybe checkNull results


loop = go 0
    where go _ _ _ [] mp = pure mp
          go carry f [] _ mp = pure mp
          go carry f (s:strs) (c:chrs) !charMap = let
            !carryRes = processFun f s c carry charMap
            in concatMap (\ ~(newCar, newMap) -> go newCar f strs chrs newMap) carryRes

standardiseInps :: [String] -> String -> ([String], String)
standardiseInps inps res = (transpose lined, normaledRes)
    where (normaledRes: lined) = map (reverse . addPadding padAmount) (res:inps)
          padAmount = maximum . map length $ (res:inps)
          addPadding amount x = replicate (amount - length x) ' ' ++ x


-- >>> pairColumns ["SEND","MORE"] "MONEY"
-- (["DE","NR","EO","SM","  "],"YENOM")
pairColumns :: [String] -> String -> ([String], [Char])
pairColumns inps = unzip . uncurry zip . standardiseInps inps


initChars :: String -> Map.Map Char [Int]
initChars xs = Map.insert ' ' [0] $ foldr initiator Map.empty (nub xs)
        where initiator x = Map.insert x [0..9]


-- >>> testInp 
-- [fromList [(' ',[0]),('D',[1]),('E',[5]),('M',[0]),('N',[3]),('O',[8]),('R',[2]),('S',[7]),('Y',[6])],fromList [(' ',[0]),('D',[1]),('E',[7]),('M',[0]),('N',[3]),('O',[6]),('R',[4]),('S',[5]),('Y',[8])],fromList [(' ',[0]),('D',[1]),('E',[8]),('M',[0]),('N',[2]),('O',[4]),('R',[6]),('S',[3]),('Y',[9])],fromList [(' ',[0]),('D',[1]),('E',[8]),('M',[0]),('N',[5]),('O',[7]),('R',[3]),('S',[6]),('Y',[9])],fromList [(' ',[0]),('D',[2]),('E',[4]),('M',[0]),('N',[3]),('O',[9]),('R',[1]),('S',[8]),('Y',[6])],fromList [(' ',[0]),('D',[2]),('E',[5]),('M',[0]),('N',[4]),('O',[9]),('R',[1]),('S',[8]),('Y',[7])],fromList [(' ',[0]),('D',[2]),('E',[7]),('M',[0]),('N',[1]),('O',[4]),('R',[6]),('S',[3]),('Y',[9])],fromList [(' ',[0]),('D',[2]),('E',[7]),('M',[0]),('N',[3]),('O',[6]),('R',[4]),('S',[5]),('Y',[9])],fromList [(' ',[0]),('D',[3]),('E',[6]),('M',[0]),('N',[4]),('O',[8]),('R',[2]),('S',[7]),('Y',[9])],fromList [(' ',[0]),('D',[3]),('E',[8]),('M',[0]),('N',[5]),('O',[7]),('R',[2]),('S',[6]),('Y',[1])],fromList [(' ',[0]),('D',[4]),('E',[3]),('M',[0]),('N',[2]),('O',[9]),('R',[1]),('S',[8]),('Y',[7])],fromList [(' ',[0]),('D',[4]),('E',[5]),('M',[0]),('N',[2]),('O',[7]),('R',[3]),('S',[6]),('Y',[9])],fromList [(' ',[0]),('D',[4]),('E',[5]),('M',[0]),('N',[3]),('O',[8]),('R',[2]),('S',[7]),('Y',[9])],fromList [(' ',[0]),('D',[5]),('E',[4]),('M',[0]),('N',[1]),('O',[7]),('R',[3]),('S',[6]),('Y',[9])],fromList [(' ',[0]),('D',[6]),('E',[3]),('M',[0]),('N',[1]),('O',[8]),('R',[2]),('S',[7]),('Y',[9])],fromList [(' ',[0]),('D',[7]),('E',[5]),('M',[1]),('N',[6]),('O',[0]),('R',[8]),('S',[9]),('Y',[2])],fromList [(' ',[0]),('D',[7]),('E',[8]),('M',[0]),('N',[1]),('O',[3]),('R',[6]),('S',[2]),('Y',[5])],fromList [(' ',[0]),('D',[9]),('E',[4]),('M',[0]),('N',[1]),('O',[7]),('R',[2]),('S',[6]),('Y',[3])],fromList [(' ',[0]),('D',[9]),('E',[4]),('M',[0]),('N',[2]),('O',[8]),('R',[1]),('S',[7]),('Y',[3])],fromList [(' ',[0]),('D',[9]),('E',[5]),('M',[0]),('N',[3]),('O',[8]),('R',[1]),('S',[7]),('Y',[4])],fromList [(' ',[0]),('D',[9]),('E',[6]),('M',[0]),('N',[4]),('O',[8]),('R',[1]),('S',[7]),('Y',[5])],fromList [(' ',[0]),('D',[9]),('E',[7]),('M',[0]),('N',[1]),('O',[4]),('R',[5]),('S',[3]),('Y',[6])],fromList [(' ',[0]),('D',[9]),('E',[8]),('M',[0]),('N',[1]),('O',[3]),('R',[6]),('S',[2]),('Y',[7])],fromList [(' ',[0]),('D',[9]),('E',[8]),('M',[0]),('N',[2]),('O',[4]),('R',[5]),('S',[3]),('Y',[7])],fromList [(' ',[0]),('D',[9]),('E',[8]),('M',[0]),('N',[4]),('O',[6]),('R',[3]),('S',[5]),('Y',[7])]]
testInp = loop addChars cols chrs mp
    where mp = initChars "SENDMOREMONEY"
          (cols, chrs) = pairColumns ["SEND","MORE"] "MONEY"

-- >>> testMap
-- Just (fromList [(' ',[0]),('b',[0,1,2,3,4,6,7,8,9]),('i',[0,1,2,3,4,6,7,8,9]),('o',[0,1,2,3,4,6,7,8,9]),('t',[5])])
testMap = traverse (\x -> if null x then Nothing else Just x) $ updateMap (initChars "tobi") [(' ',5),('t',5)]
