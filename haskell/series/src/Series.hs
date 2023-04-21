module Series (slices) where

import Data.Char

slices :: Int -> String -> [[Int]]
slices 0 [] = [[]]
slices _ [] = []
slices n list@(x:xs) | length list < n = []
                     | otherwise = (map digitToInt . take n $ list) : slices n xs
