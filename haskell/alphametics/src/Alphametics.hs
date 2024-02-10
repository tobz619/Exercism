{-# LANGUAGE GADTs #-}
module Alphametics (solve) where

import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void(Void)
import qualified Data.Map as Map
import Data.List ( delete, nub, transpose )
import Data.Char ( toUpper )
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.List.NonEmpty (nonEmpty, toList, NonEmpty ((:|)))
import Control.Monad.State (get, put, modify')
import Control.Applicative (liftA2)
import Data.Function (on)


type PossibleChars = Map.Map Char [Int]
type DigitMap = Map.Map Int Char

data Oper a where
    Val :: Oper a
    Add :: String -> String -> Oper String
    Sub :: String -> String -> Oper String

type Parser = Parsec Void String

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."

addP, subP :: Parser (Oper String)
addP = do a <- map toUpper <$> many letterChar
          _ <- space1
          _ <- char '+'
          _ <- space1
          b <- map toUpper <$> many letterChar
          return (Add a b)

subP = do a <- many letterChar
          _ <- space1
          _ <- char '-'
          _ <- space1
          b <- many letterChar
          return (Sub a b)


setChar :: Char -> Int -> PossibleChars -> PossibleChars
setChar ch v = Map.mapWithKey (selector ch)
        where selector toChange key xs
                | key == toChange = [v]
                | ch == ' ' = [0]
                | otherwise = delete v xs


sumDigits :: Integral a => [a] -> [a] -> [a]
sumDigits xs ys = reverse $ sumDigits' 0 (reverse xs) (reverse ys)
    where sumDigits' c [] []
                | c == 0 = []
                | otherwise = [c]
          sumDigits' c as [] = sumDigits as [c]
          sumDigits' c [] ys = sumDigits [c] ys
          sumDigits' c (a:as) (b:bs) =
            let (c', dig) = (c + a + b) `divMod` 10
             in dig : sumDigits' c' as bs

updateMap ::  PossibleChars -> [(Char, Int)] -> PossibleChars
updateMap = foldr (uncurry setChar)

addChars cs resChar carry charMap = do
    as <- mapM (`Map.lookup` charMap) cs
    resVals <- resChar `Map.lookup` charMap
    return [(c, v1 ,resDig) |
              v1 <- sequence as,
              (c, resDig) <- (`divMod` 10) . (carry +) <$> v1,
              resDig `elem` resVals,
              (sum v1 `mod` 10) == resDig
              ]



standardiseInps :: [String] -> String -> ([String], String)
standardiseInps inps res = (transpose lined, normaledRes)
    where (normaledRes: lined) = map (reverse . addPadding padAmount) (res:inps)
          padAmount = maximum . map length $ (res:inps)
          addPadding amount x = replicate (amount - length x) ' ' ++ x

pairColumns :: [String] -> String -> [(String, Char)]
pairColumns inps = uncurry zip . standardiseInps inps

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

-- >>> initChars "AB"
initChars :: [Char] -> Map.Map Char [Int]
initChars xs = Map.insert ' ' [0] $ foldr initiator Map.empty (nub xs :: String)
        where initiator x = Map.insert x [0..9]

-- >>> addChars "DE" 'Y' 0 (initChars "YDE")
-- Just [(0,[0,0],0),(0,[0,0],0),(0,[0,1],1),(0,[0,2],2),(0,[0,3],3),(0,[0,4],4),(0,[0,5],5),(0,[0,6],6),(0,[0,7],7),(0,[0,8],8),(0,[0,9],9),(0,[1,0],1),(0,[2,0],2),(0,[3,0],3),(0,[4,0],4),(0,[5,0],5),(0,[6,0],6),(0,[7,0],7),(0,[8,0],8),(0,[9,0],9)]


