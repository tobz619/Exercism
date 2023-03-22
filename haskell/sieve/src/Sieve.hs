module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
<<<<<<< HEAD
primesUpTo n = sieve [2..n]
            where   sieve [] = []
                    sieve (p:xs) = p : sieve [x | x <- xs, x `notElem` [p,2*p..n]]
=======
primesUpTo n = error "You need to implement this function."
>>>>>>> e84524456529c3b098a2a285dfa7d76620e771c3
