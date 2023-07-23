module Phone (number) where

import Data.Char

number :: String -> Maybe String
number xs =  checkHead (normalise xs) >>= checkNum

normalise = filter isNumber

checkHead [] = Nothing
checkHead (x:xs)
    | x < '2' && length xs == 10 = Just xs
    | length (x:xs) == 10 = Just (x:xs)
    | otherwise = Nothing

checkNum :: [Char] -> Maybe [Char]
checkNum [] = Nothing
checkNum (a:b:c:d:xs)
    | a > '1' && d > '1' = Just (a:b:c:d:xs)
    | otherwise = Nothing