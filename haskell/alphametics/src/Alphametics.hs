module Alphametics (solve) where

import Data.Char (ord, toUpper, chr)
import Data.List
import qualified Data.Map as Map
import Text.Megaparsec (many, (<|>), Parsec, runParser, parseTest, parse)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Data.Void ( Void )
import qualified Text.Megaparsec.Error
import Control.Monad (forM)


data Oper = Value String | Add Oper Oper | Sub Oper Oper | Total Oper String deriving Show

type MyParser a = Parsec Void String a

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."

operMap :: (String -> String) -> Oper -> Oper
operMap f (Value s) = Value (f s)
operMap f (Add a b) = Add (operMap f a) (operMap f b)
operMap f (Sub a b) = Sub (operMap f a) (operMap f b)
operMap f (Total a b) = Total (operMap f a) (f b)

ops :: [(MyParser a, b)] -> MyParser b
ops xs = foldr1 (<|>) op
      where op = do (p, oper) <- xs
                    return $ do _ <- p
                                return oper

addOps :: MyParser (Oper -> Oper -> Oper)
addOps = ops [ (char '+' <* space1, Add)
             , (char '-' <* space1, Sub)
             ]

equalsParser :: MyParser String
equalsParser = do _ <- string "=="
                  _ <- space1
                  many letterChar

parseValue :: MyParser Oper
parseValue = Value <$> (many letterChar <* space1)

operBuilder :: MyParser Oper
operBuilder = parseValue `chainl` addOps

exprParser :: MyParser Oper
exprParser = Total <$> operBuilder <*> equalsParser

chainl :: MyParser a -> MyParser (a -> a -> a) -> MyParser a
chainl p op = p >>= rest
            where rest x = (do f <- op
                               y <- p
                               rest (f x y)) <|> return x

chainr :: MyParser a -> MyParser (a -> a -> a) -> MyParser a
chainr p op = p >>= \x -> op >>= \f -> p `chainr` op >>= \y ->
              return (f x y) <|> return x


padLeft :: [String] -> [String]
padLeft strs = foldr (makeMax maxLen) [] strs
            where maxLen = maximum . map length $ strs
                  makeMax i inp acc = (replicate diff ' ' ++ inp) : acc
                              where diff = i - length inp

initUniques :: Oper -> Map.Map Char Int
initUniques (Value s)   = foldr (`Map.insert` 0) Map.empty s
initUniques (Add a b)   = Map.union (initUniques a) (initUniques b)
initUniques (Sub a b)   = Map.union (initUniques a) (initUniques b)
initUniques (Total a s) = Map.union (initUniques a)
                                    (initUniques (Value s))

assignValues m = Map.fromList $ go 0 (Map.toList m)
            where go _ [] = []
                  go acc ((k,_):xs) = (k,acc) : go (acc+1) xs
                  
            



{- testFuncs -}

runTotalOper :: String -> Either (Text.Megaparsec.Error.ParseErrorBundle String Void) Oper
runTotalOper = parse exprParser ""

testStatement :: Int -> String
testStatement 1 = "FIVE + SEVEN == MONEY"
testStatement 2 = "YACHT - CLUB == BOAT"
testStatement 3 =  "A + REALLY - LONG + SUM == EVERYTHING"

testParse x = runTotalOper $ testStatement x

values x = assignValues . initUniques <$> testParse x