module Spiral (spiral) where

data Spiral a = Turn a (Spiral a) | Forward a (Spiral a) | End deriving Show

spiral :: Int -> [[Int]]
spiral size = error "You need to implement this function."

fromList :: Int -> Int -> [a] -> Spiral a
fromList _ _ [] = End
fromList n t xs = go n t (n-1) xs
        where go _ _ _ [] = End
              go n 0 n' xs = fromList (n-1) 2 xs
              go n t 0 (x:xs)  = Turn x (go n (t-1) (n-1) xs)
              go n t n' (x:xs) = Forward x (go n t (n' - 1) xs)

spiralMaker :: Int -> Spiral Int
spiralMaker n = fromList n 1 [1..n^2]

spiralToLists :: Spiral a -> [[a]]
spiralToLists = go []
           where go acc End = pure acc
                 go acc (Turn a b) = (a : acc) : spiralToLists b
                 go acc (Forward a b) = go (a:acc) b