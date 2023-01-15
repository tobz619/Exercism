module Yacht (yacht, Category(..)) where

import qualified Data.Map as Map

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht deriving Eq

yacht :: Category -> [Int] -> Int
yacht Ones xs = foldr (\x -> if x == 1 then (+1) else (+0)) 0 xs
yacht Twos xs = foldr (\x -> if x == 2 then (+2) else (+0)) 0 xs
yacht Threes xs = foldr (\x -> if x == 3 then (+3) else (+0)) 0 xs
yacht Fours xs = foldr (\x -> if x == 4 then (+4) else (+0)) 0 xs
yacht Fives xs = foldr (\x -> if x == 5 then (+5) else (+0)) 0 xs
yacht Sixes xs = foldr (\x -> if x == 6 then (+6) else (+0)) 0 xs

yacht FullHouse xs = check $ count (take 5 xs)
                     where check [(a,3),(b,2)] = 3 * a + b * 2
                           check [(a,2),(b,3)] = a * 2 + b * 3
                           check _ = 0

yacht FourOfAKind xs = four $ count (take 5 xs)
                     where four = foldr (\(a,c) acc -> if c >= 4 then 4*a else 0) 0 . filter (\(_,c) -> c >= 4)

yacht LittleStraight xs
       | count xs == zip [1..5] [1,1..] = 30 
       | otherwise = 0 

yacht BigStraight xs
       | count xs == zip [2..6] [1,1..] = 30
       | otherwise = 0

yacht Choice xs = sum xs

yacht Yacht xs = check $ count xs
              where check [(_,5)] = 50
                    check _ = 0


count :: Ord a => [a] -> [(a,Int)]
count xs = Map.toList $ foldr occurs Map.empty xs
            where occurs x acc
                   | Map.lookup x acc == Nothing = Map.insert x 1 acc
                   | otherwise = Map.alter (fmap (+1)) x acc 