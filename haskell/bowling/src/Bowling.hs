module Bowling (score, BowlingError(..)) where

import Data.List

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = case checklen . sequence $ (take 10) (openScore rolls) of
                Left x -> Left x
                Right x -> Right $ sum x



checklen (Right []) = Left IncompleteGame
checklen (Right xs) | last xs == 10 && last (init xs) == 10 && length xs < 12 = Left IncompleteGame 
                    | last xs == 10 && length xs < 11 = Left IncompleteGame
                    | length xs < 10 = Left IncompleteGame
                    | otherwise = Right xs
checklen x = x


scorer :: [Int] -> Int -> Maybe (Either BowlingError Int, ([Int],Int))
scorer [10,a,b] l | a+b <= 10 = Just (Right (10+a+b), ([a,b], l+1))
                  | a < 0 = Just (Left (InvalidRoll (l+1) a), ([], l+2))
                  | b < 0 = Just (Left (InvalidRoll (l+2) b), ([], l+2))
                  | a > 10 = Just (Left (InvalidRoll (l+1) a), ([], l+2))
                  | b > 10 = Just (Left (InvalidRoll (l+2) b), ([], l+2))

scorer (a:b:c:xs) l | a == 10 = Just (Right (10+b+c), (b:c:xs, l+1))
                    | a+b == 10 = Just (Right (a+b+c), (c:xs,l+2))
                    | a > 10 = Just (Left (InvalidRoll (l) a), (c:xs, l+2))
                    | b > 10 = Just (Left (InvalidRoll (l+1) b), (c:xs, l+2))
                    | a < 0 = Just (Left (InvalidRoll (l) a), (c:xs, l+2))
                    | b < 0 = Just (Left (InvalidRoll (l+1) b), (c:xs, l+2))
                    | c < 0 = Just (Left (InvalidRoll (l+2) c), (c:xs, l+2))
                    | a+b > 10 = Just (Left (InvalidRoll (l+1) b), (c:xs, l+2))

scorer (a:b:xs) l    | a+b > 10 = Just (Left (InvalidRoll (l+1) b), (xs, l+2))
                     | a > 10 = Just (Left (InvalidRoll l a),(xs,l+2))
                     | b > 10 = Just (Left ((InvalidRoll (l+1) b)),(xs,l+2))
                     | a < 0 = Just (Left (InvalidRoll l a), (xs, l+2))
                     | b < 0 = Just (Left ((InvalidRoll (l+1) b)),(xs,l+2))
                     | otherwise = Just (Right (a+b), (xs,l+2))

scorer (a:xs) l | a < 0 = Just (Left (InvalidRoll l a), (xs,l+1))
                | a > 10 = Just (Left (InvalidRoll l a), (xs,l+1))
                | otherwise = Just (Right (a), (xs,l+1))

scorer [] _ = Nothing

openScore :: [Int] -> [Either BowlingError Int]
openScore xs = unfoldr (\(ys, c) -> (scorer ys c)) (xs,0)