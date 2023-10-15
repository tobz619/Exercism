{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree (Node, rootLabel))
import Control.Monad (msum)

reparent :: Eq a => Tree a -> Tree a -> Tree a
reparent (Node x xs) (Node y ys) = Node y (Node x xs': ys)
                        where xs' = [u | u@(Node r _) <- xs, r /= y]

tracePathTo :: Eq a => a -> Tree a -> Maybe [Tree a]
tracePathTo x t@(Node r []) | x == r = Just [t]
                            | otherwise = Nothing
tracePathTo x tree = go [] tree
            where go acc t@(Node r rs) 
                    | x == r = Just . reverse $ (t:acc)
                    | otherwise = msum $ go (t:acc) <$> rs

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = foldl1 reparent <$> tracePathTo x tree

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = 
      let newtree = fromPOV from tree
          newpath = newtree >>= tracePathTo to
       in (fmap.fmap) rootLabel newpath

