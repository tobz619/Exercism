module Spiral (spiral) where

data Spiral a = Turn a (Spiral a) | Forward a (Spiral a) | End deriving Show

spiral :: Int -> [[Int]]
spiral size = error "You need to implement this function."

makeSquares :: (Num a, Enum a) => a -> [a]
makeSquares n = [1..n^2]

spiralMaker :: Int -> Spiral Int
spiralMaker n = fromList n 1 [1..n^2]

fromList :: Int -> Int -> [a] -> Spiral a
fromList _ _ [] = End
fromList n t xs = go n t (n-1) xs
        where go _ _ _ [] = End
              go n 0 n' xs = fromList (n-1) 2 xs
              go n t 0 (x:xs)  = Turn x (go n (t-1) (n-1) xs)
              go n t n' (x:xs) = Forward x (go n t (n' - 1) xs)
