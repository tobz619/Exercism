{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

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
import Data.List.NonEmpty (NonEmpty)
import Text.Megaparsec.Error
import Text.Megaparsec.Error.Builder (err, utok)
import Control.Applicative(liftA2)


data Oper = Word String
          | Add Oper Oper
          | Sub Oper Oper
          | Total Oper String
          deriving Show

type MyParser = Parsec Void String

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."

{- Parsing functions -}
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
parseValue = Word <$> (many letterChar <* space1)

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

{- Map functions -}

frequencies :: String -> Map.Map Char Int
frequencies = Map.fromListWith (+) . map ((,1) . toUpper)

{-| Creates a Map containing Char (Value :: Int) pairs for me to reference and apply to the values into the correct order
     Does this by union'ing mini maps together and creating a map where every character is paired with a value of how many
     times it appears -}
initUniquesMap (Add a b)   = Map.unionWith (+) (initUniquesMap a) (initUniquesMap b)
initUniquesMap (Sub a b)   = Map.unionWith (+) (initUniquesMap a) (initUniquesMap b)
initUniquesMap (Total a s) = Map.unionWith (+) (initUniquesMap a) (frequencies s)
initUniquesMap (Word s)    = frequencies s


padLeft :: [String] -> [String]
padLeft strs = foldr (makeMax maxLen) [] strs
            where maxLen = maximum . map length $ strs
                  makeMax i inp acc = (replicate diff ' ' ++ inp) : acc
                              where diff = i - length inp

opChars _ ' ' a  = toUpper a
opChars _ a ' '  = toUpper a
opChars op b c   = chr (normalise b `op` normalise c)

                  where normalise = ord . toUpper



addChars =  opChars (+)
minChars = opChars (-)


opString op x y = zipWith (opChars op) xs ys
            where [xs,ys] = padLeft [x, y]


addString = opString (+)
minString = opString (-)


evalNumList [] = []
evalNumList [x] = dealWith (divMod x 10)
            where dealWith (0,y) = [y]
                  dealWith (d,m) = [d-1,m]
evalNumList (x:y:xs) = carryOver (divMod x 10)
            where carryOver (0,z) = z :evalNumList (y: xs)
                  carryOver (d,z) = d-1 : evalNumList (y+z : xs) 


{- testFuncs -}

runTotalOper :: String -> Either (ParseErrorBundle String Void) Oper
runTotalOper = parse exprParser ""

testStatement :: Int -> String
testStatement 1 = "SEND + MORE == MONEY"
testStatement 2 = "YACHT - CLUB == BOAT"
testStatement 3 =  "A + REALLY - LONG + SUM == EVERYTHING"
testStatement _ = ""


testParse :: Int -> Either (ParseErrorBundle String Void) Oper
testParse = runTotalOper . testStatement

values :: Int -> Map.Map Char Int
values x = case  initUniquesMap <$> testParse x of
            Left _ -> Map.empty
            Right mp -> mp

a = map ord "SEND"
b = map ord "MORE"
c = zipWith (+) a b
d = map (`mod` ord 'A') c