module RunLength (decode, encode) where

import Data.List (group, unfoldr)
import Data.Char

decode :: String -> String
decode encodedText = concat $ unfoldr texter (encodedText,[])

texter :: ([Char], [Char]) -> Maybe ([Char], ([Char], [Char]))
texter ([], _) = Nothing
texter (x:xs, acc) | isDigit x = Just ([], (xs,x:acc))
                   | otherwise = Just (replicate (readAcc acc) x, (xs,[]))

                   where readAcc [] = 1
                         readAcc xs = (read . reverse) xs :: Int

encode :: String -> String
encode text = concatMap asdf $ group text
            where asdf xs | length xs == 1 = xs
                          | otherwise = (show . length) xs ++ [head xs]


