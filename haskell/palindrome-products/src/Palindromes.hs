module Palindromes (largestPalindrome, smallestPalindrome) where

import Control.Applicative (liftA2)
import Data.List

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | otherwise = find (\x -> isPalindrome x && (not . null . getFactors minFactor maxFactor) x) 
                        [maxFactor ^ 2, maxFactor ^ 2 - 1 .. minFactor ^ 2] >>=
                  \x -> pure (x, getFactors minFactor maxFactor x)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | otherwise = find (\x -> isPalindrome x && (not . null . getFactors minFactor maxFactor) x) 
                        [minFactor^2 .. maxFactor^2] >>=
                         \x -> pure (x, getFactors minFactor maxFactor x)

isPalindrome :: Integer -> Bool
isPalindrome x = swapDigits x == x
                    where swapDigits n = aux n 0
                             where aux 0 y = y
                                   aux x y = let (x',y') = x `quotRem` 10
                                              in aux x' (10*y+y')


getFactors :: Integral b => b -> b -> b -> [(b, b)]
getFactors min max x = [(v,a) | v <- [min .. floor . sqrt . fromIntegral $ x]
                        , let (a,b) = x `divMod` v
                        , b == 0
                        , a >= min
                        , a <= max]