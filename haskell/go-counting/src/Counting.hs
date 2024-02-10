module Counting (
    Color(..),
    territories,
    territoryFor
) where

{-
Stolen from tobeannouncd, using it as a learning opportunity
-}

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (find)
import Debug.Trace (traceShowId)
import Data.Tuple (swap)

data Color = Black | White | Empty deriving (Eq, Ord, Show)
type Coord = (Int, Int)
data Stone = Stone {coord :: Coord, colour :: Color} deriving (Eq, Ord, Show)
type Board = Map.Map Coord Color

newtype Graph a = Graph (Map.Map a [a]) deriving (Show, Eq, Ord)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = map 
                    (\coordSet -> (S.map swap coordSet, getColour madeBoard coordSet)) 
                    (expandGroups madeBoard)
                where madeBoard = mkBoard board

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board co = find (S.member co . fst) (territories  board)

mkBoard :: [String] -> Board
mkBoard xs = Map.fromList $! go (1,1) (unlines xs) []
        where go _ [] _ = []
              go (r,_) ('\n':rest) acc = go (r+1, 1) rest acc
              go (r,c) ('B':ys) acc = ((r,c), Black) : go (r,c+1) ys acc
              go (r,c) ('W':ys) acc = ((r,c), White) : go (r,c+1) ys acc
              go (r,c) (_:ys) acc = ((r,c), Empty) : go (r,c+1) ys acc

eligibleNeighbours :: Coord -> Board -> [Coord]
eligibleNeighbours start b = mapMaybe checkColour (neighbours start)
                  where checkColour co = co <$ Map.lookup co b
                        neighbours (r,c) = [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]


getColour :: Board -> Set Coord -> Maybe Color
getColour b grp
    | S.null adj = Nothing
    | Just Black `S.member` adj 
      && Just White `S.member` adj = Nothing
    | Just Black `S.member` adj =  Just Black
    | otherwise = Just White
    where
        adj = let allNeighbours =  S.fromList 
                                    (concatMap (`eligibleNeighbours` b) grp) 
                                    `S.difference` grp 

               in S.map (`Map.lookup` b) allNeighbours
               
expandGroups :: Board -> [Set Coord]
expandGroups board = go empties
        where empties = Map.filter (== Empty) board
              go pool
               | Map.null pool = []
               | otherwise = let emptyPoint = fst $ Map.findMin pool
                                 (grp, newPool) =
                                    go' (Map.delete emptyPoint pool)
                                        (S.singleton emptyPoint)
                                        S.empty
                              in grp : go newPool

              go' pool stack found
                | S.null stack = (found, pool)
                | otherwise = let
                    adjacents = S.fromList $ concatMap (`eligibleNeighbours` board) stack
                    poolSet = S.fromList $ Map.keys pool
                    newStack = S.intersection poolSet adjacents
                    newPool = Map.filterWithKey checkKeys board
                                where checkKeys k _ = S.member k poolSet'
                                      poolSet' = S.difference poolSet newStack 
                    newFound = S.union found stack
                    in go' newPool newStack newFound

