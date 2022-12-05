module Matrix (saddlePoints) where

import Data.Array
import Data.List

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = foldr findSaddles [] (assocs matrix)
                where findSaddles x acc = let maxRow  = concatMap findMaxs (sameRow matrix)
                                              minCol  = concatMap findMins (sameCol matrix)
                                           in if x `elem` maxRow && x `elem` minCol
                                              then fst x : acc
                                              else acc


getRow :: Int -> Array (Int, Int) Int -> [((Int, Int), Int)]
getRow i arr = filter (\(coord,_) -> fst coord == i) (assocs arr)


getCol :: Int -> Array (Int, Int) Int -> [((Int, Int), Int)]
getCol i arr = filter (\(coord,_) -> snd coord == i) (assocs arr)

sameRow :: Array (Int, Int) Int -> [[((Int, Int), Int)]]
sameRow matrix = nub [getRow i matrix | i <- fst <$> indices matrix]

sameCol :: Array (Int, Int) Int -> [[((Int, Int), Int)]]
sameCol matrix = nub [getCol i matrix | i <- snd <$> indices matrix]

findMaxs :: [((Int, Int), Int)] -> [((Int, Int), Int)]
findMaxs = foldr isMax []
        where  isMax x [] = [x]
               isMax (a,x) ((b,y):acc)
                 | x > y = [(a,x)]
                 | x == y = (a,x):(b,y):acc
                 | otherwise = (b,y):acc

findMins :: [((Int, Int), Int)] -> [((Int, Int), Int)]
findMins = foldr isMin []
        where  isMin x [] = [x]
               isMin (a,x) ((b,y):acc)
                | x < y = [(a,x)]
                | x == y = (a,x):(b,y):acc
                | otherwise =(b,y):acc


saddlePoints' :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints' arr =
  [ (i, j) | i <- [1 .. m] , j <- [1 .. n] 
  , arr ! (i, j) == maxRow i
  , arr ! (i, j) == minCol j
  ]
  where (m, n) = snd $ bounds arr
        maxRow i = maximum [ arr ! (i, j') | j' <- [1 .. n] ]
        minCol j = minimum [ arr ! (i', j) | i' <- [1 .. m] ]