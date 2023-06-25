{-# LANGUAGE GADTs #-}
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


data Oper = Value String | Add Oper Oper | Sub Oper Oper | Total Oper String deriving Show

type MyParser = Parsec Void String

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."

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


initUniques :: Oper -> Map.Map Char Int
initUniques (Value s)   = foldr (`Map.insert` 0) (Map.fromList [(' ',0)]) s
initUniques (Add a b)   = Map.union (initUniques a) (initUniques b)
initUniques (Sub a b)   = Map.union (initUniques a) (initUniques b)
initUniques (Total a s) = Map.union (initUniques a)
                                    (initUniques (Value s))

assignValues m = Map.fromList <$> (sequence . go 0 $ Map.toList m)
            where go _ [] = []
                  go acc ((k,_):xs) | acc > 10 = [Nothing]
                                    | otherwise = Just (k,acc) : go (acc+1) xs


padLeft :: [String] -> [String]
padLeft strs = foldr (makeMax maxLen) [] strs
            where maxLen = maximum . map length $ strs
                  makeMax i inp acc = (replicate diff ' ' ++ inp) : acc
                              where diff = i - length inp



totalEval (Total o ans) m = operEval o m ans
      where operEval :: Oper -> Map.Map Char Int -> String -> [Maybe Int]
            operEval (Value s) m ans = let [pad,_] = padLeft [s, ans]
                                    in map (`Map.lookup` m) pad

            operEval (Add a b) m ans   = let (resA, resB) = (operEval a m ans, operEval b m ans)
                                    in zipWith (liftA2 (+)) resA resB

            operEval (Sub a b) m ans   = let (resA, resB) = (operEval a m ans, operEval b m ans)
                                    in zipWith (liftA2 (-)) resA resB
            operEval _ _ _ = []
totalEval _ _ = []

addChars ' ' ' ' c m _ = pure $ Map.alter (const (Just 1)) c m
addChars a b c m op | a == c && Map.lookup a m /= Just 0 = pure $ Map.alter (const (Just 0)) b m
                    | b == c && Map.lookup b m /= Just 0 = pure $ Map.alter (const (Just 0)) a m
                    | otherwise = let result = (`mod` 10) <$> liftA2 op (Map.lookup a m) (Map.lookup b m)
                                      existing = Map.elems m
                                      remaining = filter (`notElem` existing) [1..9]
                                      combinations = [ (newA, newB, newC `mod` 10) | newA <- remaining, newB <- remaining
                                                     , let newC = newA + newB
                                                     , newA /= newB]
                                      Just newMap = updateMapWithPotential a b c m op
                                  in if m == newMap
                                     then Just newMap
                                     else updateMapWithPotential a b c m op >>=
                                          \nextMap -> addChars a b c nextMap op


updateMapWithPotential a b c m op = case combinations of
                                     [] -> Nothing
                                     _ ->  Just newMap

            where existing = Map.elems m
                  remaining = filter (`notElem` existing) [1..9]
                  combinations = [ (newA, newB, newC) | newA <- remaining, newB <- remaining
                                 , let newC = newA `op` newB
                                 , newA /= newB]
                  newMap = let (aVal, bVal, cVal) = head combinations
                               aUp = Map.alter (const . pure $ aVal) a m
                               bUp = Map.alter (const . pure $ bVal) b aUp
                               cUp = Map.alter (const . pure $ cVal) c bUp
                            in cUp

{- testFuncs -}

runTotalOper :: String -> Either (ParseErrorBundle String Void) Oper
runTotalOper = parse exprParser ""

testStatement :: Int -> String
testStatement 1 = "FIVE + SEVEN == MONEY"
testStatement 2 = "YACHT - CLUB == BOAT"
testStatement 3 =  "A + REALLY - LONG + SUM == EVERYTHING"
testStatement _ = ""
             

testParse :: Int -> Either (ParseErrorBundle String Void) Oper
testParse = runTotalOper . testStatement

values :: Int -> Maybe (Map.Map Char Int)
values x = case  initUniques <$> testParse x of
            Left _ -> Nothing
            Right a -> Just a

someFunc = totalEval res v
            where Just v = values 1
                  Right res = testParse 1