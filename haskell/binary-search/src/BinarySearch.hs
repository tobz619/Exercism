module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find arr = bsearch arr lf ri
        where (lf, ri) = bounds arr


bsearch :: Ord a => Array Int a -> Int -> Int -> a -> Maybe Int
bsearch arr l r x
 | l > r = Nothing
 | arr ! mid > x = bsearch arr l (mid - 1) x
 | arr ! mid < x = bsearch arr (mid + 1) r x
 | otherwise = Just mid
        where mid = (l + r) `div` 2