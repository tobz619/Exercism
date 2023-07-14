module CryptoSquare (encode) where

import Data.List
import Data.Char

encode :: String -> String
encode "" =  ""
encode xs =  concat.fillLasts.transpose.breakUp.normalise $ xs

normalise :: String -> String
normalise = filter isAlphaNum . map toLower

breakUp :: String -> [String]
breakUp xs = splitEvery (ceiling ((sqrt.fromIntegral) (length xs))) xs
        where splitEvery _ [] = []
              splitEvery n ys = take n ys : splitEvery n (drop n ys)

fillLasts :: [String] -> [String]
fillLasts xs = intersperse " " $ foldr checkLength [] xs
            where checkLength [] acc = acc
                  checkLength x acc = if length x < length (head xs)
                                       then (x ++ " ") : acc
                                       else x : acc