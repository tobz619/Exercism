module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type Board a = [((Int, Int), a)]

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = error "You need to implement this function."

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = error "You need to implement this function."

mkBoard :: [String] -> Board Char
mkBoard xs = go (1,1) (unlines xs) []
        where go _ [] _ = []
              go (r,_) ('\n':rest) acc = go (r+1, 1) rest acc
              go (r,c) (y:ys) acc = ((r,c), y) : go (r,c+1) ys acc 

board5x5 = [ "  B  "
                   , " B B "
                   , "B W B"
                   , " W W "
                   , "  W  " ]
-- >>> example
-- [((1,1),' '),((1,2),' '),((1,3),'B'),((1,4),' '),((1,5),' '),((2,1),' '),((2,2),'B'),((2,3),' '),((2,4),'B'),((2,5),' '),((3,1),'B'),((3,2),' '),((3,3),'W'),((3,4),' '),((3,5),'B'),((4,1),' '),((4,2),'W'),((4,3),' '),((4,4),'W'),((4,5),' '),((5,1),' '),((5,2),' '),((5,3),'W'),((5,4),' '),((5,5),' ')]
example = mkBoard board5x5
