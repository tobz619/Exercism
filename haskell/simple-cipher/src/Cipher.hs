module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char
import System.Random
import Control.Monad
import System.Random.Stateful (randomM)

caesarDecode :: String -> String -> String
caesarDecode key = zipWith decodeLetter (cycle key)

caesarEncode :: String -> String -> String
caesarEncode key = zipWith encodeLetter (cycle key)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do rlen <- randomRIO (1,length text)
                             k <- replicateM rlen (randomRIO ('a','z'))
                             return (k, caesarEncode k text)

encodeLetter :: Char -> Char -> Char
encodeLetter k v = chr $ ord v + ((ord . toLower) k - ord 'a')

decodeLetter :: Char -> Char -> Char
decodeLetter k v | isUpper v = chr $ ord 'A' + comp
                 | otherwise =  chr $ ord 'a' + comp
                        where comp = ((ord . toLower) k - (ord . toLower) v) `mod` 26