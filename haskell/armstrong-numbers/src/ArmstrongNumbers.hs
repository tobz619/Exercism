module ArmstrongNumbers (armstrong) where

armstrong :: (Show a, Read a, Integral a) => a -> Bool
armstrong x = sum (numSum x) == x 

numSum :: (Num a, Show a, Read a) => a -> [a]
numSum x = map (^digitNum x) . toDigits $ x
        where digitNum = length . show 
              toDigits = map (read . pure) . show