module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Maybe

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum)

allergies :: Int -> [Allergen]
allergies 0 = []
allergies score = getAllergy score [Cats, Pollen .. Eggs]
                    where getAllergy _ [] = []
                          getAllergy s (x:xs) | isAllergicTo x s = x : allergies (score `rem` fromJust (lookup x getAllergenScorePairs) )
                                              | otherwise = getAllergy s xs

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo _ 0 = False
isAllergicTo allergen score = fmap (`rem` score) (lookup allergen getAllergenScorePairs) >= Just 0


getAllergenScorePairs :: [(Allergen, Int)]
getAllergenScorePairs = zip [Eggs .. Cats] (1 : map (2^) [1..])