module WordCount (wordCount) where

-- import qualified Data.Map as Map
import Data.Char


wordCount :: String -> [(String,Int)]
wordCount xs = sumOcc [] (normalise xs)
            where sumOcc acc [] = acc
                  sumOcc acc (x:xs)
                    | elem x (map fst acc) = sumOcc acc xs
                    | otherwise = sumOcc ((x , length $ filter (== x) (x:xs)) : acc) xs

normalise :: String -> [String]
normalise [] = []
normalise xs = let
    removedCommas [] = []
    removedCommas (x:xs) | x == ','  = ' ' : removedCommas xs
                         | isLetter x || isSpace x = x : removedCommas xs
                         | otherwise = x : removedCommas xs
    removedQuotes [] = ""
    removedQuotes xs
     | (not.isAlphaNum) (head xs) = removedQuotes (tail xs)
     | (not.isAlphaNum) (last xs) = removedQuotes (init xs)
     | otherwise = xs
    in filter (not . null) $ map removedQuotes (words (removedCommas (map toLower xs)))

--wordCount'' :: String -> [(String,Int)]
--wordCount'' xs = Map.toList $ foldr countWords Map.empty ((words.filter (not.isPunctuation)) xs)
--          where countWords x acc
--                  | elem x (map fst (Map.toList acc)) = Map.alter (fmap (+1)) x acc
--                  | otherwise = Map.insert x 1 (Map.fromList (Map.toList acc))
