module RailFenceCipher (encode, decode) where

import Data.List ( groupBy, sort, sortBy )
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
decode n xs = insertFrom (genPos n) letterList
            where letterList = (map.map) snd . groupPos . seed n $ xs

seed :: Int -> [b] -> [(Int, b)]
seed i xs =  zip (sort . take (length xs) . genPos $ i) xs

groupPos :: [(Int, b2)] -> [[(Int, b2)]]
groupPos = groupBy ((==) `on` fst)

finder :: Int -> [[a]] -> Maybe (a, [[a]])
finder n = go n []
      where go 1 acc ((f:fs):gs) = Just (f, reverse acc ++ fs : gs)
            go v acc (fs:gs) = go (v-1) (fs:acc) gs
            go _ _ [] =  Nothing

insertFrom :: [Int] -> [[a]] -> [a]
insertFrom [] _ = []
insertFrom (n:ns) gs = case finder n gs of
            Just (res, newlist) -> res : insertFrom ns newlist
            Nothing -> []