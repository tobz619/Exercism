module Knapsack (maximumValue) where
import Data.Function (on)
import Data.List (sortBy, tails, maximumBy)

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue _ [] = 0
maximumValue n items = sum . map snd . getMax . f n . valuedSort $ items
    where valuedSort = sortBy (compare `on` snd)
          getMax = maximumBy (compare `on` (sum . map snd))
          f maxWeight xs = undefined


testWeight :: [(Int, Int)]
testWeight = [
            (25, 350),
            (35, 400),
            (45, 450),
            (5, 20),
            (25, 70),
            (3, 8),
            (2, 5),
            (2, 5)
            ]