module Bowling (score, BowlingError(..)) where

import Data.List

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = case sequence $ openScore rolls of
                Left x -> Left x
                Right x -> Right $ sum x


scorer :: [Int] -> Int -> Maybe (Either BowlingError Int, ([Int],Int))
scorer (a:b:c:xs) l | a == 10   = Just (Right (a+b+c), (b:c:xs, l+1))
                    | a+b == 10 = Just (Right (a+b+c), (c:xs,l+2))
                    | a > 10 = Just (Left (InvalidRoll (l) b), (c:xs, l+2))
                    | b > 10 = Just (Left (InvalidRoll (l+1) b), (c:xs, l+2))
                    | a+b > 10 = Just (Left (InvalidRoll (l+1) b), (c:xs, l+2))

scorer (a:b:xs) l    | a > 10 = Just (Left ((InvalidRoll l a)),(xs,l+2))
                     | a+b > 10 = Just (Left ((InvalidRoll (l+1) b)),(xs,l+2))
                     | b > 10 = Just (Left ((InvalidRoll (l+1) b)),(xs,l+2))
                     | otherwise = Just (Right (a+b), (xs,l+2))

scorer [] _ = Nothing

openScore :: [Int] -> [Either BowlingError Int]
openScore xs = unfoldr (\(ys, c) -> (scorer ys c)) (xs,1)