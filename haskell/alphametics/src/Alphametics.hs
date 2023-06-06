module Alphametics (solve) where

import Data.Char (ord, toUpper, chr)
import qualified Data.Map as Map
import Text.Megaparsec (many, (<|>), Parsec, runParser)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Control.Applicative ()
import Data.Void ( Void )


data Oper = Values String | Add Oper Oper | Sub Oper Oper

type MyParser a = Parsec Void String a

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."


evalExpr (Values s) = s
evalExpr (Add a b) = zipWith getCharDifference (evalExpr a) (evalExpr b)
evalExpr (Sub a b) = zipWith getCharDifference (evalExpr a) (evalExpr b)

addOps :: MyParser (Oper -> Oper -> Oper )
addOps = add <|> minus
        where add = char '+' >> return Add
              minus = char '-' >> return Sub

matchLen :: String -> String -> (String, String)
matchLen a b | diff < 0  = ( replicate (negate diff) ' ' ++ a, b )
             | otherwise = ( a, replicate diff ' ' ++ b )
                  where diff = length a - length b
                    

exprParser :: MyParser (Oper -> Oper -> Oper, [Char], [Char])
exprParser = do word1 <- many letterChar
                _ <- space
                op <- addOps
                _ <- space 
                word2 <- many letterChar
                let (w1,w2) = matchLen word1 word2
                return (op, w1, w2)

diffParser :: MyParser Oper
diffParser = do (f,a,b) <- exprParser
                return $ f (Values a) (Values b)

getList = (evalExpr <$>) . runParser (lexeme diffParser) "Failed to parse"


getCharOffset :: Char -> Int
getCharOffset c = (ord . toUpper) c - ord 'A'

getValuePairs = map (\x -> (x, getCharOffset x))

getCharDifference :: Char -> Char -> Int
getCharDifference c ' ' = c 
getCharDifference ' ' d = d
getCharDifference c d | (ord . toUpper) c + getCharOffset d > ord 'Z' = ord c + getCharOffset d - 26
                      | otherwise = ord c + getCharOffset d

seeSumn = map getValuePairs ["five", "seven"]

sumnMap = Map.fromList (concat seeSumn)

sc :: MyParser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: MyParser a -> MyParser a
lexeme = L.lexeme sc