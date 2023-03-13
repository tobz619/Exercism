module RotationalCipher (rotate) where

import Data.Char

rotate :: Int -> String -> String
rotate int = map (shift int)

shift :: Int -> Char -> Char
shift int x | isLower x = chr $ ord 'a' + ((ord x - ord 'a' + int) `mod` 26)
            | isUpper x = chr $ ord 'A' + ((ord x - ord 'A' + int) `mod` 26)
            | otherwise = x
