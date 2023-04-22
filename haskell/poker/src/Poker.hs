module Poker (bestHands) where

import Data.Char
import Data.Functor
import Control.Applicative

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show, Eq, Enum)

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Ord, Enum, Bounded)

type Card = (Value, Suit)

data Hand = RoyalFlush | StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | OnePair | HighCard deriving (Show, Eq, Ord)

bestHands :: [String] -> Maybe [String]
bestHands = error "You need to implement this function!"


fromValue :: String -> Maybe Value
fromValue ""   = Nothing
fromValue "A" = Just Ace
fromValue "J"  = Just Jack
fromValue "Q"  = Just Queen
fromValue "K"  = Just King
fromValue xs = let value = read xs :: Int
                   k i | i > 10 = Nothing
                       | i < 0 = Nothing
                       | otherwise = lookup value valueTable
                            where valueTable = zip [2 .. 10] [Two .. Ten]
                in k value

fromSuit :: String -> Maybe Suit
fromSuit "S" = Just Spades
fromSuit "H" = Just Hearts
fromSuit "D" = Just Diamonds
fromSuit "C" = Just Clubs
fromSuit _ = Nothing

toCard :: String -> Maybe Card
toCard xs = liftA2 (,) (fromValue num) (fromSuit suit)
            where (num, suit) = span (\x -> (`elem` "AJQK") x || isDigit x) xs

toCards :: String -> Maybe [Card]
toCards = traverse toCard . words

scoreHand :: [Card] -> Maybe Hand
scoreHand cards = checkHands cards $> handTest cards
                where checkHands cs | length cs /= 5 = Nothing
                                    | otherwise = Just cards
scoreHandSt :: String -> Maybe Hand
scoreHandSt xs = toCards xs >>= scoreHand

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

handTest :: [Card] -> Hand
handTest cards | isRoyalFlush cards = RoyalFlush
               | isStraightFlush cards = StraightFlush
               | isFourOfAKind cards = FourOfAKind
               | isFullHouse cards = FullHouse
               | isFlush cards = Flush
               | isStraight cards = Straight
               | isThreeOfAKind cards = ThreeOfAKind
               | isTwoPair cards = TwoPair
               | isPair cards = OnePair
               | otherwise = HighCard

exampleFullHouse = fives ++ kings
                where fives = [(Five, s) | s <- [Spades, Clubs, Hearts] ]
                      kings = [(King, s) | s <- [Spades, Clubs]]

someHand = [ "4H 7H 8H 9H 6H"
                               , "2S 4S 5S 6S 7S"]

someExample = map scoreHandSt someHand