{-#LANGUAGE OverloadedStrings #-}
module Atbash (decode, encode) where

import qualified Data.Text as T
import           Data.Text (Text)
import Data.Char (isUpper, toLower, isSpace, isAlphaNum)

decode :: Text -> Text
decode = T.filter isAlphaNum . encode

encode :: Text -> Text
encode = T.unwords . T.chunksOf 5 . T.foldr encodeChar "" 

alphabet :: Text
alphabet = "abcdefghijklmnopqrstuvwxyz"

backwards :: Text
backwards = T.reverse alphabet

pairs :: [(Char, Char)]
pairs = T.zip alphabet backwards

encodeChar :: Char -> Text -> Text
encodeChar a acc
    | not . isAlphaNum $ a = acc
    | isUpper a = maybe (a `T.cons` acc) (`T.cons` acc) (lookup (toLower a) pairs)
    | otherwise = maybe (a `T.cons` acc) (`T.cons` acc) (lookup a pairs)

