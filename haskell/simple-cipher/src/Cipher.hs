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

{-| Shifts the letter in the v by k: makes some checks via up and j to make sure the shift is within bounds -}
encodeLetter :: Char -> Char -> Char
encodeLetter k = letterShift (intShift k)

{-| Same as encode except subtracts letterShift -}
decodeLetter :: Char -> Char -> Char
decodeLetter k = letterShift (negate . intShift $ k)                     

{-| Calculates how a letter is shifted relative to A -}
intShift :: Char -> Int
intShift a | isUpper a = ord a - ord 'A'
           | otherwise = ord a - ord 'a'


{-| Shifts letters according to the int given modulo 26. If it exceeds the bounds then
    subtract 26 to get the correct position -}
letterShift :: Int -> Char -> Char
letterShift i c | isAlpha c = up shift
                | isLower c = lw shift
                | otherwise = c 
                     where shift = ord c + (i `mod` 26)
                           up l | not . isAlpha . chr $ l = up . (26 `subtract`) $ l 
                                | otherwise = chr l
                           lw l | not . isLower . chr $ l = lw . (26 `subtract`) $ l 
                                | otherwise = chr l