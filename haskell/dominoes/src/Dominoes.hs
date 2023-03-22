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