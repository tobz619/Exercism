{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Frequency (frequency) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Char (isAlpha, toLower)
import Control.Concurrent
import Control.Parallel
import Control.Monad.State

frequency :: Int -> [T.Text] -> Map.Map Char Int
frequency nWorkers texts = letters (removeSpace texts) Map.empty

removeSpace :: [T.Text] -> String
removeSpace = map toLower . filter isAlpha . concatMap T.unpack


-- | Counts the number of occurences of the first letter 
--   and returns a new string omitting that letter
--   and a map containing a count of that letter
--
-- >>> runState (letterFreq "Apple") Map.empty
-- ("pple",fromList [('A',1)])
--
letterFreq :: String -> State (Map.Map Char Int) String 
letterFreq xs = state $ \mp -> isLet xs mp (safeHead xs)
            where safeHead (x:_) = Just x
                  safeHead  _    = Nothing

                  isLet str acca (Just x)  =  let !singl = Map.insert x (length . filter (== toLower x) 
                                                            . map toLower $ str) acca

                                                  !filtered = filter (/= x) str
                                               in filtered `par` (singl `pseq`
                                                  (filtered,singl))
                  
                  isLet _ acca Nothing =  ([], acca)

letters ::  String -> Map.Map Char Int -> Map.Map Char Int
letters xs acc  = case remainder of
              [] -> mp
              _ -> letters remainder mp
            where (!remainder, !mp) =  runState (letterFreq xs) acc            
