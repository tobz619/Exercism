module Dominoes (chain) where

import Data.Tuple
import Data.List

type Domino = (Int,Int)

chain :: [Domino] -> Maybe [Domino]
chain [] = Just []
chain ds = k . filter sameFirstLast . filter (\x -> length x == length ds) . mkChain $ ds
            where k [] = Nothing
                  k (x:_) = Just x

sameFirstLast :: [Domino] -> Bool
sameFirstLast [] = False
sameFirstLast xs = (fst . head) xs == (snd . last) xs

validNexts :: Domino -> [Domino] -> [(Domino, [Domino])]
validNexts d@(l,r) domList = [ k valid | valid <- domList, let (l',r') = valid,
                                        l' == l || l' == r ||
                                        r' == l || r' == r]
                        
                              where k d = (d, delete d domList)


appendToChain :: Domino -> [Domino] -> [Domino]
appendToChain d [] = [d]
appendToChain d@(l,r) ds@((l',_):_)  | l == l' = swap d:ds
                                     | r == l' = d:ds
                                     | otherwise = ds



mkChain ds = go [] ds
         where go acc [] = pure acc
               go acc ds = do d <- ds
                              (res, rest) <- validNexts d ds
                              go (appendToChain res acc) rest



{- 1. Take every domino and run it with validNexts to get all valid neighbours.
   2. If it validNexts returns Nothing, leave that domino in the potential chain list;
      if Just xs then remove the first occurrence of that domino from the list.
   3. Run non-deterministically.
   4. Ensure all 
-}