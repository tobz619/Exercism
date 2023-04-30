module Diamond (diamond) where

import Data.Maybe
import Data.Char

diamond :: Char -> Maybe [String]
diamond x | not . isAlpha $ x  = Nothing
          | otherwise = pure $ fullBuilder x


quarterBuilder :: Char -> [String]
quarterBuilder x = go [] x 0
            where go acc 'A' c = ('A' : replicate c ' ') : acc
                  go acc cha c = let newAcc = fmap (' ':) ((cha:replicate c ' '):acc)
                                  in go newAcc (pred cha) (c+1)

halfBuilder :: Char -> [String]
halfBuilder x = quarterBuilder x >>= \line -> pure $ (reverse.tail) line ++ line

fullBuilder :: Char -> [String]
fullBuilder x = halfBuilder x ++ (tail . reverse . halfBuilder) x