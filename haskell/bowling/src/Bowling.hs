module Bowling (score, BowlingError(..)) where

import Data.List

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data Score = X | Spare | Score Int | Blank

data GameCounter = GameCounter {bc :: BC,
                                fc :: FC,
                                sc :: Either BowlingError Int} deriving (Show, Eq)

type FC = Int

type BC = Int

score :: [Int] -> Either BowlingError Int
score [] = Left IncompleteGame
score rolls = do res <- checkRolls rolls
                 finalScore res zero

zero :: GameCounter
zero =  GameCounter 0 1 (Right 0)

incBalls :: Int -> GameCounter -> GameCounter
incBalls inc g = g {bc = bc g + inc }

incFrame :: GameCounter -> GameCounter
incFrame g = g {fc = fc g + 1 }

updateScore :: Either BowlingError Int -> GameCounter -> GameCounter
updateScore (Left x) g = case sc g of
                          Left _ -> g
                          _ -> g {sc = Left x}
updateScore (Right res) g = g {sc = fmap (+ res) (sc g)}


strikeUpdate :: Either BowlingError Int -> GameCounter -> GameCounter
strikeUpdate res = incBalls 1 . incFrame . updateScore res

regUpdate :: Either BowlingError Int -> GameCounter -> GameCounter
regUpdate res = incBalls 2 . incFrame . updateScore res

incomplete :: GameCounter -> GameCounter
incomplete g = updateScore (Left IncompleteGame) g 

mkF :: GameCounter -> [Int] -> Maybe (GameCounter, [Int])
mkF g [a]
  | fc g == 10 = sLast g [a] 
  | otherwise = Just (regUpdate(pure a) g, [])

mkF g (a:b:c:xs)
 | fc g == 10 = sLast g (a:b:c:xs)
 | a == 10 = Just (strikeUpdate (pure $ a + b + c) g, b:c:xs)
 | a + b == 10 = Just (regUpdate (pure $ a + b + c) g, c:xs)
 | a + b > 10 = Just(regUpdate (Left $ InvalidRoll (bc g + 1) b) g, c:xs)
 | otherwise = Just (regUpdate (pure $ a + b) g, c:xs)

mkF g (a:b:xs) 
  | fc g == 10 = sLast g (a:b:xs) 
  | a + b > 10 = Just (regUpdate (Left $ InvalidRoll (bc g + 1) b) g, xs)
  | otherwise = Just (regUpdate (pure $ a + b) g, xs)

mkF g [] = Nothing

sLast :: GameCounter -> [Int] -> Maybe (GameCounter, [Int])
sLast g [10] = Just (incomplete g, [])
sLast g [10,10] = Just (incomplete g, [])
sLast g [10,10,c]
 | c <= 10 = Just (regUpdate (pure $ 20 + c) g, [])
 | otherwise = Just (regUpdate (Left $ InvalidRoll (bc g + 2) c) g, [])
sLast g [a,b]
 | a + b == 10 = Just (incomplete g, [])
 | a == 10 = Just (incomplete g, [b])
 | otherwise = Just (regUpdate (pure $ a + b) g, [])
sLast g (a:b:c:xs)
 | length (a:b:c:xs) > 3 = Just (regUpdate (Left $ InvalidRoll (bc g + 3) (head xs)) g, [])
 | a == 10 = Just (strikeUpdate (pure $ a) g, (b:c:xs))
 | a+b == 10 = Just (regUpdate (pure $ a + b + c) g, [])
 | a+b < 10  = Just (regUpdate (Left $ InvalidRoll (bc g + 2) c) g, [])
 | otherwise = Just (regUpdate (pure $ a + b) g, [])
sLast _ [] = Nothing 





stateGame :: [Int] -> GameCounter -> (GameCounter, [Int])
stateGame xs init = let res = mkF init xs
                     in case res of Just (counter, rem) -> stateGame rem counter
                                    Nothing -> (init, xs)

finalScore :: [Int] -> GameCounter -> Either BowlingError Int
finalScore xs init = if fc (fst (stateGame xs init)) < 10 && isRight (sc (fst (stateGame xs init)))
                      then Left IncompleteGame
                      else sc . fst $ stateGame xs init
                        where isRight (Right x) = True
                              isRight _ = False


eligRoll :: Int -> Maybe Int
eligRoll 10 = Just 10
eligRoll i | i < 0 = Nothing
           | i > 10 = Nothing
           | otherwise = Just i


checkRolls :: [Int] -> Either BowlingError [Int]
checkRolls [] = Right []
checkRolls xs = sequence $ findFail (map eligRoll xs) 0
              where findFail (Just x:ys) c = Right x : findFail ys (c+1)
                    findFail (Nothing:ys) c = Left (InvalidRoll c (xs !! c)) : findFail ys (c+1)
                    findFail [] _ = []


inp1 :: [Int]
inp1 =  [10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

inp2 :: [Int]
inp2 = replicate 12 10

inp3 :: [Int]
inp3 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

inp4 :: [Int]
inp4 = [6, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

inp5 :: [Int]
inp5 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

inp6 ::[Int]
inp6 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]

inp7 :: [Int]
inp7 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 7]

inp8 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10] :: [Int]