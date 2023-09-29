{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree (Node, rootLabel), drawTree)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (msum)
import Data.List (union, (\\))

data Crumb a = Crumb a [Tree a] [Tree a] deriving Show
type Zipper a = (Tree a, Crumb a)

reparent :: Eq a => Tree a -> Tree a -> Tree a
reparent (Node x xs) (Node y ys) = Node y (Node x xs': ys)
                        where xs' = [u | u@(Node r _) <- xs, r /= y]

tracePathTo :: Eq a => a -> Tree a -> Maybe [Tree a]
tracePathTo x t@(Node r []) | x == r = Just [t]
                            | otherwise = Nothing
tracePathTo x tree@(Node r rs) = go [] tree
                  where go acc t@(Node r rs) | x == r = Just . reverse $ (t:acc)
                                             | otherwise = msum $ go (t:acc) <$> rs

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = foldl1 reparent <$> tracePathTo x tree

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = fromPOV from tree >>= \newtree ->
                                tracePathTo to newtree >>= \path ->
                                return $ map rootLabel path
