{-# LANGUAGE GADTs #-}
module Alphametics (solve) where

import Text.Megaparsec.Char 
import Text.Megaparsec
import Data.Void(Void)
import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Maybe (fromMaybe)

data Oper a where
    Add :: String -> String -> Oper String
    Sub :: String -> String -> Oper String

type Parser = Parsec Void String 

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."

addP, subP :: Parser (Oper String)
addP = do a <- map toUpper <$> many letterChar
          _ <- space1
          _ <- char '+'
          _ <- space1
          b <- map toUpper <$> many letterChar
          return (Add a b)

subP = do a <- many letterChar
          _ <- space1
          _ <- char '-'
          _ <- space1
          b <- many letterChar
          return (Sub a b)


setChar :: Char -> Int -> Map.Map Char [Int] -> Map.Map Char [Int]
setChar ch v = Map.mapWithKey (selector ch)
        where selector toChange key xs
                | key == toChange && v == 0 = [0]
                | key == toChange = [v]
                | otherwise = delete v xs   