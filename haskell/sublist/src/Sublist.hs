module Sublist (sublist) where

sublist :: Ord a => [a] -> [a] -> Maybe Ordering
sublist [] [] = Just EQ
sublist [] _ = Just LT
sublist _ [] = Just GT
sublist a b | a == b = Just EQ
            | length a > length b && b `elem` chunksOf (length b) a = Just GT
            | length b > length a && a `elem` chunksOf (length a) b = Just LT
            | otherwise = Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go
        where go xs = case splitAt n xs of
                        (a,b) | null a -> []
                              | length (a++b) < n -> []
                              | otherwise -> a : go (tail (a++b))
