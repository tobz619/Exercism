module RailFenceCipher (encode, decode) where

import Data.List
import Data.Function (on)

encode :: Int -> String -> String
encode n xs =  concat . foldr inserter start $ seeded
            where start =  replicate n []
                  seeded = zip (genPos n) xs
                  inserter (pos, v) = insertAt pos v

genPos :: (Num a, Enum a) => a -> [a]
genPos n = cycle $ nums ++ (reverse . drop 1 . init) nums
            where nums = [1..n]

insertAt :: (Eq t1, Num t1) => t1 -> t2 -> [[t2]] -> [[t2]]
insertAt 1 el (x:xs) = (el:x):xs
insertAt n el (x:xs) = x : insertAt (n-1) el xs
insertAt _ _ [] = []

decode :: Int -> String -> String
decode n xs = concat . foldr inserter start $ reseeded
            where start =  replicate n []
                  seeded = zip (sort . take (length xs) . genPos $ n) xs
                  reseeded = concat . transpose . groupBy ((==) `on` fst) $ seeded
                  inserter (pos, v) = insertAt pos v
