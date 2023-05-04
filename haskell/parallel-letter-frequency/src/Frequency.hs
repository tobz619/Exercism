{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Frequency (frequency) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Char (isAlpha, toLower)
import Control.Parallel
import Control.Parallel.Strategies
import Data.Foldable (Foldable(foldl'))

frequency :: Int -> [T.Text] -> Map.Map Char Int
frequency nWorkers texts = foldP nWorkers (Map.unionWith (+)) (map (letterFreq2.format) texts)

              where format = filter isAlpha . map toLower . T.unpack

                    foldP :: (NFData k, NFData a, Ord k, Num a) => Int -> (Map.Map k a -> Map.Map k a -> Map.Map k a) -> [Map.Map k a] -> Map.Map k a
                    foldP _    _  [] = Map.empty

                    foldP 0 f (x:xs) = runEval $ do 
                                                y <- rpar x
                                                rpar $ f y (foldP nWorkers f xs)
                                                 
                    
                    foldP cores f (x:xs) = runEval $ do
                                                y <- rparWith rdeepseq x
                                                ys <- rpar $ foldP (cores - 1) f xs
                                                rpar $ y `f` ys

-- | Counts the number of occurences of the first letter 
--   and returns a new string omitting that letter
--   and a map containing a count of that letter
letterFreq2 :: String -> Map.Map Char Int
letterFreq2 = Map.fromListWith (+) . map (, 1)



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