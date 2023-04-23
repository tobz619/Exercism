module Poker (bestHands) where

import Data.Char ( isDigit )
import Control.Applicative ( Applicative(liftA2) )
import Data.Maybe ( fromMaybe )
import Data.List ( groupBy, sortBy )

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show, Eq, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum, Bounded)

type Card = (Value, Suit)

bestHands :: [String] -> Maybe [String]
bestHands xs = compareHands xs >>= \wins -> pure (fmap cardsToString wins)


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

cardsToString :: [Card] -> String
cardsToString = init . concatMap ((++ " ") . fromCard)

fromCard :: Card -> String
fromCard (v, s) = fromValue v ++ fromSuit s

fromValue :: Value -> String
fromValue x = fromMaybe "" (lookup x pairTable)
                where pairTable = zip [Two .. Ace] ["2","3","4","5","6","7","8","9","10","J","Q","K","A"]

fromSuit :: Suit -> String
fromSuit x = fromMaybe "" (lookup x pairTable)
                where pairTable = zip [Spades .. ] ["S","H","D","C"]

highHands :: [[Card]] -> [[Card]]
highHands xs = findHigh handtests xs
            where findHigh [] cs = cs
                  findHigh (t:ts) cs | any t xs = filter t cs
                                     | otherwise = findHigh ts cs
                  handtests = [isRoyalFlush, isStraightFlush, isFourOfAKind, isFullHouse, isFlush, isStraight, isFiveHighStraight, isThreeOfAKind, isTwoPair, isPair]

grouper :: [Card] -> [Card]
grouper xs | or (($ xs) <$> grouptests) = concat . sortBy (\a b -> compare (length b) (length a)) . groupBy (\x y -> fst y == fst x) . sortBy (\a b -> compare (fst b) (fst a)) $ xs
           | otherwise = sortBy (\a b -> compare (fst b) (fst a)) xs
               where grouptests = [isFourOfAKind, isFullHouse, isThreeOfAKind, isTwoPair, isPair]

getHighHands :: [String] -> Maybe [[Card]]
getHighHands xs = highHands <$> (traverse toCards xs >>= checkLen)
                      where checkLen ret | all (\x -> length x == 5) ret = pure ret
                                         | otherwise = Nothing


maxHighCards :: [Card] -> [Card] -> [Card]
maxHighCards l r = checker (grouper l) (grouper r)
                  where checker [] [] = l
                        checker lef [] = lef
                        checker [] rig = rig
                        checker ((lv,_):ls) ((rv,_):rs) | lv > rv = l
                                                        | rv > lv = r
                                                        | otherwise = checker ls rs


compareHands :: [String] -> Maybe [[Card]]
compareHands xs = k $ getHighHands xs
            where k Nothing = Nothing
                  k (Just list) = let maxC = foldr1 maxHighCards list
                                   in Just $ filter (\v -> map fst maxC == map fst v) list

runUp, runDown :: Value -> Value
runUp Ace   = Two 
runUp v     = succ v

runDown Two = Ace
runDown v   = pred v

hasNeighbour :: Value -> [Card] -> Bool
hasNeighbour value vs = (\x -> x `elem` map fst vs && x /= value) (runUp value)
                      || (\x -> x `elem` map fst vs && x /= value) (runDown value)

isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards@(x:_) = cards == [ (v, anchorSuit) |
                                      let anchorSuit = snd x,
                                      let suits = map snd cards,
                                      v <- [Ace, King, Queen, Jack, Ten]
                                    ]

isStraightFlush :: [Card] -> Bool
isStraightFlush cards@(x:_) = cards == [(v, anchorSuit) |
                                          let anchorSuit = snd x,
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
isStraight cards = cards == [neighbours | neighbours <- cards, hasNeighbour (fst neighbours) cards] && (not . isFiveHighStraight) cards

isFiveHighStraight :: [Card] -> Bool
isFiveHighStraight cards = map fst (grouper cards) == Ace : [Five, Four .. Two ]

getPair :: Card -> [Card] -> Bool
getPair _ [] = False
getPair (v,_) cards = length (filter (== v) (map fst cards)) == 2

isTwoPair :: [Card] -> Bool
isTwoPair xs = length (filter (`getPair` xs) xs) == 4

isPair :: [Card] -> Bool
isPair xs = length (filter (`getPair` xs) xs) == 2

isFullHouse :: [Card] -> Bool
isFullHouse xs = isThreeOfAKind xs && isPair xs