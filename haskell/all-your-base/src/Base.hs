module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase i o inputDigits = 
    fromBase i inputDigits >>= toBase o


fromBase base is = (sum <$>) . sequence $ go 0 (reverse is)
        where go _ [] | base <= 1 = [Left InvalidInputBase]
                      | otherwise = []
              go pos (x:xs) | base <= 1 = [Left InvalidInputBase]
                            | x < 0 = [Left $ InvalidDigit x]
                            | x >= base = [Left $ InvalidDigit x]
                            | otherwise = Right (x*base^pos) : go (pos+1) xs
              

toBase base = (reverse <$>) . sequence . go
                where go 0   = []
                      go n | base <= 1 = [Left InvalidOutputBase]
                           | otherwise = Right r : go d
                              where (d,r) = quotRem n base