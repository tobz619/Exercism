module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2..n]
            where   sieve [] = []
                    sieve (p:xs) = p : sieve [x | x <- xs, x `notElem` [p,2*p..n]]
