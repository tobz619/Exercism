module Counting (
    Color(..),
    territories,
    territoryFor
) where


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence(Seq, ViewL(..))
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable (find)
import Control.Applicative (Applicative(liftA2))
import Data.Maybe (mapMaybe)
import Control.Monad.State
import Control.Monad.Reader

data Color = Black | White | Empty deriving (Eq, Ord, Show)
type Coord = (Int, Int)
data Stone = Stone {coord :: Coord, colour :: Color} deriving (Eq, Ord, Show)

data BFS = BFS { queue :: Seq Coord
               , seen :: Set Coord
               , connected :: Set Coord }
               deriving (Show, Eq)

newBFS :: BFS
newBFS = BFS Seq.empty Set.empty Set.empty

type Board = Map.Map Coord Color

newtype Graph a = Graph (Map.Map a [a]) deriving (Show, Eq, Ord)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = undefined

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board co = undefined


neighbourCoords :: Coord -> [Coord]
neighbourCoords (r,c) = [(x,y) | x <- [r-1 .. r+1], y<- [c-1 .. c+1] , (x,y) /= (r,c)]

getStoneNeighbours :: Map.Map Coord Color -> Stone -> [Coord]
getStoneNeighbours board st = mapMaybe (`sameColour` board) . neighbourCoords $ coord st
                where sameColour co b = do nCol <- Map.lookup co b
                                           if nCol == colour st
                                            then Just co
                                            else Nothing

mkBoard :: [String] -> Board
mkBoard xs = Map.fromList $! go (1,1) (unlines xs) []
        where go _ [] _ = []
              go (r,_) ('\n':rest) acc = go (r+1, 1) rest acc
              go (r,c) ('B':ys) acc = ((r,c), Black) : go (r,c+1) ys acc
              go (r,c) ('W':ys) acc = ((r,c), White) : go (r,c+1) ys acc
              go (r,c) (_:ys) acc = ((r,c), Empty) : go (r,c+1) ys acc

newBFSearch :: Stone -> State BFS ()
newBFSearch st = modify (\bfs -> bfs { queue = searchFrom })
    where searchFrom = Seq.singleton (coord st)

popQueue :: State (Seq Coord) (Maybe Coord)
popQueue = do q <- gets Seq.viewl
              case q of
                Seq.EmptyL -> return Nothing
                a :< rest -> do put rest
                                return (Just a)

addToSeen :: Coord -> Reader (Set Coord) (Set Coord)
addToSeen co = asks $ Set.insert co

addToConnected :: Coord -> Reader (Set Coord) (Set Coord)
addToConnected co = asks $ Set.insert co

extendQueue :: Foldable t => Map.Map Coord Color -> t Coord -> Stone -> Reader (Seq Coord) (Seq Coord)
extendQueue board seenList st = do
    let neighbours = Set.fromList $ getStoneNeighbours board st
        unseen =  Set.filter (`notElem` seenList) neighbours
        newQueue que = foldr (flip (Seq.|>)) que unseen
    asks newQueue

executeBFS :: Map.Map Coord Color -> Stone -> State BFS (Set Coord)
executeBFS board st = do
    q <- gets queue
    viewed <- gets seen
    c <- gets connected
    let (res, newQueue) = runState popQueue q
    case res of
        Nothing -> return c
        Just co -> do let newSeen = runReader (addToSeen co) viewed
                          moreQueue = runReader (extendQueue board newSeen st) newQueue
                          newConnected = runReader (addToConnected co) c
                      put $ BFS moreQueue newSeen newConnected
                      executeBFS board st

wholeBFS :: Map.Map Coord Color -> Stone -> State BFS (Set Coord)
wholeBFS board st = do newBFSearch st
                       executeBFS board st


board5x5 = [ "  B  "
            , " B B "
            , "B W B"
            , " W W "
            , "  W  " ]

test = evalState (wholeBFS (mkBoard board5x5) (Stone (3,4) White)) newBFS