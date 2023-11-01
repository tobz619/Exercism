module Say (inEnglish) where
import Control.Applicative (liftA2, liftA3, Alternative ((<|>)))

inEnglish :: Integer -> Maybe String
inEnglish n = foldr1 (<|>)  $ 
                fmap ($ n) [sayBillion, sayMillion, sayThousand, sayHundred, sayTens]
{- (<|>) is a cool function, essentially, if the expression on the left is Just then it keeps it
    else, if it is Nothing, if the thing on the Right is Just, it replaces it with it -}

numtable :: [(Integer, String)]
numtable = [
    (0,"zero"),
    (1,"one"),
    (2,"two"),
    (3,"three"),
    (4,"four"),
    (5,"five"),
    (6,"six"),
    (7,"seven"),
    (8,"eight"),
    (9,"nine"),
    (10,"ten"),
    (11,"eleven"),
    (12,"twelve"),
    (13,"thirteen"),
    (14,"fourteen"),
    (15,"fifteen"),
    (16,"sixteen"),
    (17,"seventeen"),
    (18,"eighteen"),
    (19,"nineteen"),
    (20,"twenty"),
    (30,"thirty"),
    (40,"forty"),
    (50,"fifty"),
    (60,"sixty"),
    (70,"seventy"),
    (80,"eighty"),
    (90,"ninety")
    ]

sayTens :: Integer -> Maybe String
sayTens x 
    | x <= 20 = lookup x numtable
    | otherwise = do t <- lookup (ten * 10) numtable
                     u <- lookup rema numtable
                     return (t ++ "-" ++ u)
                        where (ten, rema) = quotRem x 10

sayHundred :: Integer -> Maybe String
sayHundred x
    | x > 1000 || x < 100 = Nothing
    | x == 100 = Just "one hundred"
    | otherwise = do let (h, rema) = quotRem x 100
                     h <- lookup h numtable
                     fmap ((h ++ " hundred ") ++)  (sayTens rema)

sayHunTen :: Integer -> Maybe String
sayHunTen x = sayHundred x <|> sayTens x


addAnd :: Integer -> Maybe String
addAnd x | x == 0 = Just ""
         | otherwise = fmap (" " ++) (sayHunTen x)

sayThousand :: Integer -> Maybe String
sayThousand x
 | x >= 10^6 || x < 10^3 = Nothing
 | x == 10^3 = Just "one thousand"
 | otherwise = let (thou, rest) = quotRem x 1000
                in do thous <- sayHunTen thou
                      rema <- addAnd rest
                      return $ thous ++ " thousand" ++ rema

sayMillion :: Integer -> Maybe String
sayMillion x
 | x >= 10^9 || x < 10^6 = Nothing
 | x == 10^6 = Just "one million"
 | otherwise = let (mill, rest) = quotRem x (10^6)
                in do milli <- sayHunTen mill
                      rema <- inEnglish rest
                      return $ milli ++ " million " ++ rema 

sayBillion :: Integer -> Maybe String
sayBillion x
 | x >= 10^12 || x < 10^9 = Nothing
 | x == 10^9 = Just "one billion"
 | otherwise = let (bill, rest) = quotRem x (10^9)
                in do billi <- sayHunTen bill
                      rema <- inEnglish rest
                      return $ billi ++ " billion " ++ rema 