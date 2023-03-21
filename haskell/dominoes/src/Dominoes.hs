module Dominoes (chain) where

import Data.Tuple
import Data.List
import Data.Maybe

type Domino = (Int,Int)

chain :: [Domino] -> Maybe [Domino]
chain [] = Just []
chain ds = k . filter sameFirstLast . solver $ ds
         where k [] = Nothing
               k (x:_) = Just x

sameFirstLast :: [Domino] -> Bool
sameFirstLast [] = False
sameFirstLast xs = (fst . head) xs == (snd . last) xs


appendToChain :: Domino -> [Domino] -> Maybe (Domino,[Domino])
appendToChain d [] = Just (d,[d])
appendToChain d@(l,r) ds@((l',_):_)  | l == l' = Just (d , swap d:ds)
                                     | r == l' = Just (d, d:ds)
                                     | otherwise = Nothing


eligibleChainBuilder :: [Domino] -> [Domino] -> [(Domino, [Domino])]
eligibleChainBuilder acc = mapMaybe (`appendToChain` acc)

solver :: [Domino] -> [[Domino]]
solver = go [] 
      where go acc [] = pure acc
            go acc ds = do (added, newAcc) <- eligibleChainBuilder acc ds
                           go newAcc (delete added ds)

twelve :: [(Int, Int)]
twelve = [(1, 2), (5, 3), (3, 1), (1, 2), (2, 4), (1, 6), (2, 3), (3, 4), (5, 6), (3,6), (4,5), (2,1)]

three :: [(Int, Int)]
three = [(1, 2), (3, 1), (2, 3)]

{- 1. Take every domino and run it with validNexts to get all valid neighbours.
   2. If it validNexts returns Nothing, leave that domino in the potential chain list;
      if Just xs then remove the first occurrence of that domino from the list.
   3. Run non-deterministically.
   4. Ensure all 
-}