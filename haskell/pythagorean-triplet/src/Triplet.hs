module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(a,b,c) | b <- [1..sum], a <- [1..b], let c = sum - a - b, a^2 + b^2 == c^2]
