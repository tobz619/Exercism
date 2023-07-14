module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows x = map concat $ take x $ iterate toNext [[1]]

toNext :: (Num a, Eq a) => [[a]] -> [[a]]
toNext [] = []
toNext [[]] = []
toNext [[1]] = [[1],[1]]
toNext ([1]:x:xs) = [1] : fromTwo ([1]:x:xs)
toNext [[x],[1]] = [x+1] : [[1]]
toNext (x:xs) = fromTwo (x:xs)

fromTwo :: (Num a, Eq a) => [[a]] -> [[a]]
fromTwo [] = []
fromTwo [x] = [x]
fromTwo (x:y:xs) = ((+) <$> x <*> y) : fromTwo (y:xs)

-- alt method:
next :: [Integer] -> [Integer]
next xs = zipWith (+) (0 : xs) (xs ++ [0])

pascal :: [[Integer]]
pascal = iterate next [1]

rows' :: Int -> [[Integer]]
rows' n = take n pascal