module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, catMaybes)
import Control.Monad.State
import Data.List (unfoldr)

data Color = Black | White | Empty deriving (Eq, Ord, Show)
type Coord = (Int, Int)
data Stone = Stone {coord :: Coord, colour :: Color} deriving (Eq, Ord, Show)
type Board a = Map.Map Coord a

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = error "You need to implement this function."

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = error "You need to implement this function."

mkBoard :: [String] -> Board Color
mkBoard xs = Map.fromList $ go (1,1) (unlines xs) []
        where go _ [] _ = []
              go (r,_) ('\n':rest) acc = go (r+1, 1) rest acc
              go (r,c) ('B':ys) acc = ((r,c), Black) : go (r,c+1) ys acc
              go (r,c) ('W':ys) acc = ((r,c), White) : go (r,c+1) ys acc
              go (r,c) (_:ys) acc = ((r,c), Empty) : go (r,c+1) ys acc

board5x5 =  [ "  B  "
            , " B B "
            , "B W B"
            , " W W "
            , "  W  " ]
-- >>> example
-- fromList [((1,1),Empty),((1,2),Empty),((1,3),Black),((1,4),Empty),((1,5),Empty),((2,1),Empty),((2,2),Black),((2,3),Empty),((2,4),Black),((2,5),Empty),((3,1),Black),((3,2),Empty),((3,3),White),((3,4),Empty),((3,5),Black),((4,1),Empty),((4,2),White),((4,3),Empty),((4,4),White),((4,5),Empty),((5,1),Empty),((5,2),Empty),((5,3),White),((5,4),Empty),((5,5),Empty)]
example :: Board Color
example = mkBoard board5x5

--- >>> stoneNeighbours (Stone (3,3) Black) example
-- [Stone {coord = (3,2), color = Empty},Stone {coord = (2,3), color = Empty},Stone {coord = (3,4), color = Empty},Stone {coord = (4,3), color = Empty}]
stoneNeighbours :: Stone -> Board Color -> [Stone]
stoneNeighbours (Stone (r,c) _) b = mapMaybe checkColour [lf, up, ri, dw]
            where lf = (r,c-1)
                  up = (r-1,c)
                  ri = (r,c+1)
                  dw = (r+1,c)
                  checkColour co = Stone co <$> Map.lookup co b

-- >>> sameColourNeighbours (Stone (3,3) Black) example
-- []
sameColourNeighbours :: Stone -> Board Color -> [Stone]
sameColourNeighbours s b = filter ((== colour s) . colour) (stoneNeighbours s b)

enemyColour :: Color -> Maybe Color
enemyColour Black = Just White
enemyColour White = Just Black
enemyColour _ = Nothing

isConnectedToEnemy :: Board Color -> Stone -> Bool
isConnectedToEnemy b (Stone (r,c) col) = enemyColour col `elem` map (`Map.lookup` b) [lf, up, ri, dw]
                                    where lf = (r,c-1)
                                          up = (r-1,c)
                                          ri = (r,c+1)
                                          dw = (r+1,c)



-- ProgressCancelledException
stepSearch :: Board Color -- The current Board
           -> [Coord]  -- Coord queue
           -> [Stone] -- Stones of interest
           -> Maybe ([Stone], [Coord]) -- Tuple of seen coords and new queue
stepSearch _ _ [] = Nothing
stepSearch b seen (x:xs) = Just (xs ++ new, coord x: seen)
                where new =
                        filter (and .sequence [(`notElem` seen) . coord, (`notElem` xs)])
                            $ stoneNeighbours x b <> concatMap (`stoneNeighbours` b) xs



bfs :: Board Color -> Stone -> [Coord] -> [[Stone]]
bfs b initialStone = unfoldr (\q -> stepSearch b q [initialStone]) 