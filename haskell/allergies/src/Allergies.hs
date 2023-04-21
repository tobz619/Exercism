module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Maybe
import Data.List

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
allergies score = reverse $ subtLargest score -- tests only passed with reverse order? Not ideal!


isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo _ 0 = False
isAllergicTo allergen score = allergen `elem` allergies score

subtLargest :: Int -> [Allergen]
subtLargest = unfoldr (`scoreBuilder` [Cats, Pollen .. Eggs]) 

scoreBuilder :: Int -> [Allergen] -> Maybe (Allergen, Int)
scoreBuilder 0 _ = Nothing
scoreBuilder _ [] = Nothing
scoreBuilder score (x:xs) | lookup x mkAllergenScorePairs <= Just score = Just (x, score - fromJust (lookup x mkAllergenScorePairs))
                          | otherwise = scoreBuilder score xs


mkAllergenScorePairs :: [(Allergen, Int)]
mkAllergenScorePairs = zip [Eggs .. Cats] (1 : map (2^) [1..])