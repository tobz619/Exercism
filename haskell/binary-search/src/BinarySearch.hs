module BinarySearch (find) where

import Data.Array
<<<<<<< HEAD
import Data.Array.Base (safeIndex)

find :: Ord a => Array Int a -> a -> Maybe Int
find arr = queryCenter (assocs arr)


queryCenter :: Ord a => [(Int, a)] -> a -> Maybe Int
queryCenter [] _ = Nothing
queryCenter a  x = mid `compareWith` x
        
                where compareWith (Just comp) val | snd comp == val = Just . fst $ comp
                                                  | otherwise = queryNext (split a) x
                      
                      compareWith Nothing _ = Nothing
                      
                      mid = saferIndex (length a `div` 2) a

queryNext :: Ord a => ([(Int, a)], [(Int, a)]) -> a -> Maybe Int
queryNext ([],_) _ = Nothing
queryNext (_,[]) _ = Nothing
queryNext (l,r) x | x < (snd . head) r = queryCenter l x
                  | otherwise = queryCenter r x
                

split :: Ord b => [(Int, b)] -> ([(Int,b)], [(Int,b)])
split a = (left, right)
        where (left,right) = (,) (take (length a `div` 2) a) (drop (length a `div` 2) a)


saferIndex _ [] = Nothing
saferIndex 0 (b:_) = Just b
saferIndex a (_:bs) = saferIndex (a-1) bs 
=======

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = error "You need to implement this function."
>>>>>>> e84524456529c3b098a2a285dfa7d76620e771c3
