module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)

scoreLetter :: Char -> Integer
scoreLetter letter | letterVal "aeioulnrst" = 1
                   | letterVal "dg" = 2
                   | letterVal "bcmp" = 3
                   | letterVal "fhvwy" = 4
                   | letterVal "k" = 5
                   | letterVal "jx" = 8
                   | letterVal "qz" = 10
                   | otherwise = 0
                
                where letterVal = (toLower letter `elem`)

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
