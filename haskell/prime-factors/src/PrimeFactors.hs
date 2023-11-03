module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n  = primer n primes
           where primer num (p:ps)
                    | num < 2 = []
                    | num < p^2 = [num]
                    | num `mod` p == 0 = p : primer (num `div` p) (p:ps)
                    | otherwise = primer num ps

                

primes :: [Integer]
primes = f [2..]
    where f (x:xs) = x : f [p | p <- xs, p `mod` x /= 0]