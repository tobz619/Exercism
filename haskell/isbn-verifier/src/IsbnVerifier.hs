module IsbnVerifier (isbn) where

import Data.Char (isNumber, digitToInt)
import Data.List

isbn :: String -> Bool
isbn = validisbn .fmap noEarlyTen . onlyValids

onlyValids :: [Char] -> Maybe [Int]
onlyValids =  sequence . lenTen . filter isValid

            where lenTen xs = if length xs == 10 then map chartoInt xs else []
                  chartoInt 'X' = Just 10
                  chartoInt x | x `elem` ['0'..'9'] = Just $ digitToInt x
                              | otherwise = Nothing

                  isValid x = isNumber x || x == 'X'

noEarlyTen :: (Eq a, Num a) => [a] -> [a]
noEarlyTen xs | 10 `notElem` take 9 xs = xs
              | otherwise = []


validisbn :: Maybe [Int] -> Bool
validisbn (Just []) = False
validisbn (Just xs) = (== 0) . (`mod` 11). sum . zipWith (*) [10, 9 .. 1] $ xs
validisbn _ = False