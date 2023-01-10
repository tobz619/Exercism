module Raindrops (convert) where

convert :: Int -> String
convert n = check $ soundList n
            where check (Just "") = show n
                  check Nothing = show n
                  check (Just xs) = xs

soundList :: Integral a => a -> Maybe [Char]
soundList x = fmap concat . sequence . filter (/= Nothing) $ [f3Sound, f5Sound, f7Sound] <*> pure x

f3Sound :: Integral a => a -> Maybe String
f3Sound x
 | factor 3 x = Just "Pling"
 | otherwise = Nothing

f5Sound :: Integral a => a -> Maybe String
f5Sound x 
 | factor 5 x = Just "Plang"
 | otherwise = Nothing

f7Sound :: Integral a => a -> Maybe String
f7Sound x
 | factor 7 x = Just "Plong"
 | otherwise = Nothing

factor :: Integral a => a -> a -> Bool
factor x y | y `mod` x == 0 = True
           | otherwise = False