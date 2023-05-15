module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char
import System.Random
import Control.Monad

{-| Same as encode but using decodeLetter function. -}
caesarDecode :: String -> String -> String
caesarDecode key = zipWith decodeLetter (cycle key)

{-| Make an infinite list of the key and use zipWith to apply encodeKey so each character of the code 
    with its corresponding cipher character -}
caesarEncode :: String -> String -> String
caesarEncode key = zipWith encodeLetter (cycle key)

{-| Get a random length of key rlen, a random string of length rlen and return a key and an encoded text -}
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do rlen <- randomRIO (1,length text)
                             k <- replicateM rlen (randomRIO ('a','z'))
                             return (k, caesarEncode k text)

{-| Shifts the letter in v by k using positive letterShift -}
encodeLetter :: Char -> Char -> Char
encodeLetter k = letterShift (intShift k)

{-| Same as encode except subtracts letterShift -}
decodeLetter :: Char -> Char -> Char
decodeLetter k = letterShift (negate . intShift $ k)                     

{-| Calculates how much a letter is shifted relative to A -}
intShift :: Char -> Int
intShift a | isUpper a = ord a - ord 'A'
           | otherwise = ord a - ord 'a'


{-| Shifts letters recursively while (i != 0) -}
letterShift :: Int -> Char -> Char
letterShift 0 c = c
letterShift i c | i < 0 = letterShift (i `mod` 26 + 1) (shiftBackward c)
                | otherwise = letterShift (i `mod` 26 - 1) (shiftForward c)

shiftForward :: Char -> Char
shiftForward 'z' = 'a'
shiftForward 'Z' = 'A'
shiftForward  x  = succ x

shiftBackward :: Char -> Char
shiftBackward 'a' = 'z'
shiftBackward 'A' = 'Z'
shiftBackward  x  = pred x