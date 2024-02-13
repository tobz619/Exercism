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
                where selector _ ' ' _ = [0]
                      selector toChange key xs
                            | key == toChange = [v]
                            | otherwise = delete v xs


checkNull :: (Int, PossibleChars) -> Maybe (Int, PossibleChars)
checkNull (c,mp) = (,) c <$> traverse (\x -> if null x then Nothing else Just x) mp


addChars :: String -> Char -> Int -> Map.Map Char [Int] -> Maybe [(Int, PossibleChars)]
addChars cs resChar carry charMap = do
    as <- mapM (`Map.lookup` charMap) cs
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
          go carry f cols@(s:strs) res@(c:chrs) !charMap = let
            !carryRes = (carry, cols,res, charMap) `traceShow` processFun f s c carry charMap
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
-- []
testInp = loop addChars cols chrs mp
    where mp = initChars "SENDMOREMONEY"
          (cols, chrs) = pairColumns ["SEND","MORE"] "MONEY"

-- >>> testMap
-- Just (fromList [(' ',[0]),('b',[0,1,2,3,4,6,7,8,9]),('i',[0,1,2,3,4,6,7,8,9]),('o',[0,1,2,3,4,6,7,8,9]),('t',[5])])
testMap = traverse (\x -> if null x then Nothing else Just x) $ updateMap (initChars "tobi") [('t',5),('t',5)]
