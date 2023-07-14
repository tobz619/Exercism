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
factors x = [n | n <- [1..x `div` 2], x `mod` n == 0]

isPrime :: Int -> Bool
isPrime x
    | length (factors x) == 1 = True
    | otherwise = False
