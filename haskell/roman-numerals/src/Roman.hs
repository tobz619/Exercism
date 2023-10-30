module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals = fmap concat . sequence .
    numerbuild [1000,900,500,400,100,90,50,40,10,9,5,4,1]

numer :: Integer -> Maybe String
numer 1    = Just "I"
numer 4    = Just "IV"
numer 5    = Just "V"
numer 9    = Just "IX"
numer 10   = Just "X"
numer 40   = Just "XL"
numer 50   = Just "L"
numer 90   = Just "XC"
numer 100  = Just "C"
numer 400  = Just "CD"
numer 500  = Just "D"
numer 900 =  Just "CM"
numer 1000 = Just "M"
numer _ = Nothing

numerbuild :: [Integer] -> Integer -> [Maybe String]
numerbuild [] _ = []
numerbuild vs@(n:ns) num
    | num < 0 = []
    | num < n = numerbuild ns num 
    | otherwise = numer n : numerbuild vs (num - n) 
    
