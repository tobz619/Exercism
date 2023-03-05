{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Frequency (frequency) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Char (isAlpha, toLower)
import Control.Concurrent
import Control.Parallel
import Control.Parallel.Strategies

frequency :: Int -> [T.Text] -> Map.Map Char Int
frequency nWorkers texts = foldl (Map.unionWith (+)) Map.empty (runEval . parMap letterFreq2 $ map format texts)

                            where parMap _ [] = return []
                                  parMap f (x:xs) = do
                                    a <- rpar $ f x
                                    as <- parMap f xs
                                    return (a:as)
                                  
                                  format =  filter isAlpha . map toLower .T.unpack


letterFreq2 :: String -> Map.Map Char Int
letterFreq2 = foldl mapMaker Map.empty
         where mapMaker acc x = case Map.lookup x acc of
                                 Just _ -> Map.alter (fmap (+1)) x acc
                                 Nothing -> Map.insert x 1 acc


-- -- | Counts the number of occurences of the first letter 
-- --   and returns a new string omitting that letter
-- --   and a map containing a count of that letter
-- --
-- -- >>> runState (letterFreq "Apple") Map.empty
-- -- ("pple",fromList [('A',1)])
-- --
-- letterFreq :: String -> State (Map.Map Char Int) String 
-- letterFreq xs = state $ \mp -> isLet xs mp (safeHead xs)
--             where safeHead (x:_) = Just x
--                   safeHead  _    = Nothing

--                   isLet str acca (Just x)  =  let !singl = Map.insert x (length . filter (== toLower x) 
--                                                             . map toLower $ str) acca

--                                                   !filtered = filter (/= x) str
--                                                in filtered `par` (singl `pseq`
--                                                   (filtered,singl))

--                   isLet _ acca Nothing =  ([], acca)