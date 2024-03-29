module Series (Error(..), largestProduct) where
import Text.Read (readMaybe)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits = 
    mkProducts size digits >>= foldr1 getMaximum


productString :: String -> Either Error Integer
productString inp = product <$> traverse digithdlr inp
        where digithdlr val = maybe (Left $ InvalidDigit val) Right (readMaybe [val])

getSeriesOf :: Int -> String -> Either Error [String]
getSeriesOf n xs = case [take n xs' | xs' <- tails xs, length (take n xs') == n] of
                    [] -> Left InvalidSpan
                    ns -> Right ns


mkProducts :: Int -> String -> Either Error [Either Error Integer]
mkProducts size inp = getSeriesOf size inp >>= \strings ->
                      return $ productString <$> strings
                    
getMaximum :: Ord b => Either a b -> Either a b -> Either a b
getMaximum (Left x) _ = Left x
getMaximum _ (Left x) = Left x
getMaximum (Right x) (Right y) | y > x = Right y
                               | otherwise = Right x
