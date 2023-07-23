module PerfectNumbers (classify, Classification(..)) where

import Control.Monad

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify 1 = Just Deficient
classify 2 = Just Deficient
classify x
 | x <= 0 = Nothing
 | x == sum (factors x) = Just Perfect
 | x > sum (factors x) = Just Deficient
 | x < sum (factors x) = Just Abundant
 | otherwise = Nothing

factors :: Int -> [Int]
factors 0 = [0]
factors 1 = [1]
factors x = init $ foldr isFac [] [1..x]
        where isFac a acc  = if x `mod` a == 0
                                then  a : acc
                                else acc
