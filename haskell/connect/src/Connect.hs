{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}
module Connect (Mark(..), winner) where

import Data.Tuple
import Data.Char
import Control.Applicative
import Debug.Trace
import Data.Maybe

data Mark = Cross | Nought deriving (Eq, Show)
type Position = (Int, Int)
type Board = [(Position, Char)]

winner :: [String] -> Maybe Mark
winner inp = let
    board = mkCoords inp
    (maxR, maxC) = maxBoardDimensions board
    (firstO, firstX) = (getFirstVal 'O' board, getFirstVal 'X' board)
    xPath = (length <$>) . listToMaybe $ stepUntilMaxC maxC 'X' board [firstX]
    oPath = (length <$>) . listToMaybe $ stepUntilMaxR maxR 'O' board [firstO]
    findWinner Nothing Nothing = Nothing
    findWinner a b
        | a < b = Just Cross
        | otherwise = Just Nought
    in findWinner oPath xPath

mkCoords :: [String] -> [(Position, Char)]
mkCoords inp = swap <$> go (1,1) (unlines . map (filter (not . isSpace)) $ inp)
        where go (r, _) ('\n':rest) = go (r+1, 1) rest
              go (r,c) (v:xs)     = (v, (r,c)) : go (r,c+1) xs
              go _ [] = []

surround :: Position -> [Position]
surround coord = map (\f -> f coord)
    [ topLeft, topRight
    , left, right
    , botLeft, botRight
    ]
        where topLeft (r,c) = (r-1, c)
              topRight (r,c) = (r-1,c+1)
              left (r,c) = (r,c-1)
              right (r,c) = (r, c+1)
              botLeft (r,c) = (r+1, c-1)
              botRight (r,c) = (r+1, c)



continue :: Eq p => p -> [(Position, p)] -> [Position] -> [[Position]]
continue targ board stack  =
    case eligs of
        [] -> []
        vs -> [ v:stack | v <- vs ]
    where eligs = filter (and . sequence [(`isEligCoord` board), (`notElem` stack)]) neighbours
          neighbours = surround (head stack)
          isEligCoord pos b = case (targ ==) <$> lookup pos b of
                    Just True -> True
                    _ -> False



getFirstVal :: (Eq b, Eq a, Num b, Num a) => Char -> [((a, b), Char)] -> [(a, b)]
getFirstVal 'X' board =  filter (\(coord, char) -> and [snd coord == 1, char == 'X']) board
                    >>= \v -> [fst v]
getFirstVal 'O' board = filter (\(coord, char) -> and [fst coord == 1, char == 'O']) board
                    >>= \v -> [fst v]
getFirstVal _ _ = []

step :: (Foldable t, Eq p) => p -> [(Position, p)] -> t [Position] -> [[Position]]
step targ board = concatMap (continue targ board)


stepUntilMaxC, stepUntilMaxR :: Eq t => Int -> t -> [(Position, t)] -> [[Position]] -> [[(Int, Int)]]
stepUntilMaxC _ _ _ [] = []
stepUntilMaxC _ _ _ [[]] = []
stepUntilMaxC maxV targ board stacks =
    if any ((== maxV) . snd . head) stacks
    then filter ((== maxV) . snd . head) stacks
    else let newStacks = step targ board stacks
          in stepUntilMaxC maxV targ board newStacks

stepUntilMaxR _ _ _ [] = []
stepUntilMaxR _ _ _ [[]] = []
stepUntilMaxR maxV targ board stacks =
    if any ((== maxV) . fst . head) stacks
    then filter ((== maxV) . fst . head) stacks
    else let newStacks = step targ board stacks
          in stepUntilMaxR maxV targ board newStacks


maxBoardDimensions :: Board -> (Int, Int)
maxBoardDimensions [] = (0,0)
maxBoardDimensions b = (maxX, maxY)
        where coords = map fst b
              maxX = maximum . map fst $ coords
              maxY = maximum . map snd $ coords

