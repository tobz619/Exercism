{-# LANGUAGE BangPatterns #-}
module Dominoes (chain) where

import Data.Tuple
import Data.List
import Data.Maybe
import Debug.Trace

type Domino = (Int,Int)

chain :: [Domino] -> Maybe [Domino]
chain = find sameFirstLast . solver

sameFirstLast :: [Domino] -> Bool
sameFirstLast [] = True
sameFirstLast xs = fromMaybe False $
                    uncons xs >>= \(d, _) ->
                    unsnoc xs >>= \(_, d') ->
                    pure (fst d == snd  d')
    
  where unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing


appendToChain :: [Domino] -> Domino -> Maybe (Domino, [Domino])
appendToChain [] d = Just (d, [d])
appendToChain ds d@(l,r) = do
  top <- listToMaybe ds
  check top
    where check (l',_)
            | l == l' = Just (d, swap d:ds)
            | r == l' = Just (d, d:ds)
            | otherwise = Nothing


eligibleChainBuilder :: [Domino] -> [Domino] -> [(Domino, [Domino])]
eligibleChainBuilder acc = mapMaybe (appendToChain acc)


solver :: [Domino] -> [[Domino]]
solver = go []
  where go !acc [] = pure (traceShowId acc)
        go !acc !ds = do (d, newAcc) <- eligibleChainBuilder acc ds
                         go newAcc (delete d ds)
