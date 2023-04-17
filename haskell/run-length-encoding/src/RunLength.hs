module RunLength (decode, encode) where

import Data.List (group, unfoldr)
import Data.Char

decode :: String -> String
decode encodedText = concat $ unfoldr (`texter` []) encodedText

texter [] _ = Nothing
texter (x:xs) acc | isDigit x = Just (x:acc, xs)
                  | otherwise = Just (replicate (read acc :: Int) x, xs)

encode :: String -> String
encode text = concatMap asdf $ group text
            where asdf xs | length xs == 1 = xs
                          | otherwise = (show . length) xs ++ [head xs]


