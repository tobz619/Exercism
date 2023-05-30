module Alphametics (solve) where

import Data.Char (ord, toUpper, chr)
import Text.Megaparsec (many, (<|>), Parsec, runParser)
import Text.Megaparsec.Char
import Control.Applicative ()
import Data.Void ( Void )


data Oper = Add [Int] [Int] | Sub [Int] [Int]

type MyParser a = Parsec Void String a

evalExpr :: Oper -> [Int]
evalExpr (Add a b) = zipWith (+) a b
evalExpr (Sub a b) = zipWith (-) a b

addOps :: MyParser ( [Int] -> [Int] -> Oper )
addOps = add <|> minus
        where add = char '+' >> return Add
              minus = char '-' >> return Sub

exprParser :: MyParser ([Int] -> [Int] -> Oper, [Int] , [Int])
exprParser = do word1 <- many letterChar
                _ <- space
                op <- addOps
                _ <- space 
                word2 <- many letterChar
                return (op, ord <$> word1, ord <$> word2)

diffParser :: MyParser Oper
diffParser = do (f,a,b) <- exprParser
                return $ f a b

getList = (evalExpr <$>) . runParser diffParser ""

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."

getCharOffset :: Char -> Int
getCharOffset c = (ord . toUpper) c - ord 'A'

getCharDifference c d = chr . abs $ getCharOffset c - getCharOffset d
