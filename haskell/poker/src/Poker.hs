module Poker (bestHands) where

import Data.Char
import Control.Applicative

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show, Eq, Enum)

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Ord, Enum, Bounded)

data Hand = RoyalFlush | StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | OnePair | HighCard deriving (Show, Eq, Ord, Enum)

type Card = (Value, Suit)

bestHands :: [String] -> Maybe [String]
bestHands xs = compareHands xs >>= \x -> pure (fmap cardsToString x)


tovalue :: String -> Maybe Value
tovalue ""   = Nothing
tovalue "A" = Just Ace
tovalue "J"  = Just Jack
tovalue "Q"  = Just Queen
tovalue "K"  = Just King
tovalue xs = let value = read xs :: Int
                 k i | i > 10 = Nothing
                     | i < 0 = Nothing
                     | otherwise = lookup value valueTable
                         where valueTable = zip [2 .. 10] [Two .. Ten]
                in k value

toSuit :: String -> Maybe Suit
toSuit "S" = Just Spades
toSuit "H" = Just Hearts
toSuit "D" = Just Diamonds
toSuit "C" = Just Clubs
toSuit _ = Nothing

toCard :: String -> Maybe Card
toCard xs = liftA2 (,) (tovalue num) (toSuit suit)
            where (num, suit) = span (\x -> (`elem` "AJQK") x || isDigit x) xs

toCards :: String -> Maybe [Card]
toCards = traverse toCard . words

getHighHands :: [String] -> Maybe [[Card]]
getHighHands xs = highHands <$> (mapM toCards xs >>= checkLen)
                      where checkLen ret | all (\x -> length x == 5) ret = pure ret
                                         | otherwise = Nothing

highHands :: [[Card]] -> [[Card]]
highHands xs = findHigh handtests xs
            where findHigh [] xs = xs
                  findHigh (t:ts) xs | (not . any t) xs = findHigh ts xs
                                     | otherwise = filter t xs

                  handtests = [isRoyalFlush, isStraightFlush, isFourOfAKind, isFullHouse, isFlush, isStraight, isThreeOfAKind, isTwoPair, isPair, isHighCard]

sortHand :: [Card] -> [Card]
sortHand [] = []
sortHand [x] = [x]
sortHand (first@(val,_):rest) = smaller ++ pure first ++ bigger
                          where smaller = sortHand $ filter (\(v,_) -> v < val) rest
                                bigger  = sortHand $ filter (\(v,_) -> v > val) rest

maxHighCards :: [Card] -> [Card] -> [Card]
maxHighCards l r = checker (sortHand l) (sortHand r)
                  where checker ((lv,_):ls) ((rv,_):rs) | lv > rv = l
                                                        | rv > lv = r
                                                        | otherwise = checker ls rs
                        checker [] [] = l

compareHands :: [String] -> Maybe [[Card]]
compareHands xs = k $ getHighHands xs
            where k Nothing = Nothing
                  k (Just [xs]) = Just [xs]
                  k (Just list@(c1:cs)) = let max = map fst $ foldr maxHighCards c1 cs
                                           in Just $ filter (\x -> map fst x == max) list

cardsToString :: [Card] -> String
cardsToString = init . concatMap ((++ " ") . fromCard)

fromCard :: Card -> String
fromCard (v, s) = fromValue v ++ fromSuit s

fromValue :: Value -> String
fromValue x = maybe "" id (lookup x pairTable)
                where pairTable = zip [Ace .. King] ["A","2","3","4","5","6","7","8","9","10","J","Q","K"]

fromSuit :: Suit -> String
fromSuit x = maybe "" id (lookup x pairTable)
                where pairTable = zip [Spades .. ] ["S","H","D","C"]

runUp, runDown :: Value -> Value
runUp v   | v == maxBound = maxBound
          | otherwise = succ v

runDown v | v == minBound = minBound
          | otherwise = pred v

hasNeighbour :: Value -> [Card] -> Bool
hasNeighbour value vs = (`elem` map fst vs) (runUp value)
                      || (`elem` map fst vs) (runDown value)

isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards@(x:_) = cards == [ (v, anchorSuit) |
                                      let anchorSuit = snd x,
                                      let suits = map snd cards,
                                      let values = map fst cards,
                                      v <- [Ace, King, Queen, Jack, Ten]
                                    ]

isStraightFlush :: [Card] -> Bool
isStraightFlush cards@(x:_) = cards == [(v, anchorSuit) |
                                          let anchorSuit = snd x,
                                          let ,
                                          v <- map fst cards,
                                          hasNeighbour v cards]

isXOfAKind :: Int -> [Card] -> Bool
isXOfAKind i cards@(x:xs)  | length cards < i = False
                            | length (filter (\(v,_) -> (== v) (fst x)) cards) == i = True
                            | otherwise = isXOfAKind i xs

isFourOfAKind :: [Card] -> Bool
isFourOfAKind = isXOfAKind 4

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind = isXOfAKind 3

isFlush :: [Card] -> Bool
isFlush cards@(x:_) = cards == [(v, anchorSuit) | let anchorSuit = snd x, v <- map fst cards]

isStraight :: [Card] -> Bool
isStraight cards = cards == [neighbours | neighbours <- cards, hasNeighbour (fst neighbours) cards]

getPair :: Card -> [Card] -> Bool
getPair _ [] = False
getPair _ [x] = False
getPair (v,_) cards = length (filter (== v) (map fst cards)) == 2

isTwoPair :: [Card] -> Bool
isTwoPair xs = length (filter (`getPair` xs) xs) == 4

isPair :: [Card] -> Bool
isPair xs = length (filter (`getPair` xs) xs) == 2

isFullHouse :: [Card] -> Bool
isFullHouse xs = isThreeOfAKind xs && isPair xs

isHighCard :: [Card] -> Bool
isHighCard = const True

exampleFullHouse = fives ++ kings
                where fives = [(Five, s) | s <- [Spades, Clubs, Hearts] ]
                      kings = [(King, s) | s <- [Spades, Clubs]]

someHand = [ "3H 6H 7H 8H 5H"
                                , "4S 5H 4C 5D 4H"]

singleHand = ["4S 5S 7H 8D JC"]

multiWin = [ "4D 5S 6S 8D 3C"
                               , "2S 4C 7S 9H 10H"
                               , "3S 4S 5D 6H JH"
                               , "3H 4H 5C 6C JD"]