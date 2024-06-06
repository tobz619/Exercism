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
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad.State
import Control.Monad.Reader
import Debug.Trace

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)
data Stone = Stone {coord :: Coord, colour :: Color} deriving (Eq, Ord, Show)

data BFS = BFS { queue :: Seq Coord
               , seen :: Set Coord
               , connected :: Set Coord }
               deriving (Show, Eq)

newBFS :: BFS
newBFS = BFS Seq.empty Set.empty Set.empty

type Board = Map.Map Coord (Maybe Color)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = Set.toList $ Set.fromList $ mapMaybe (territoryFor board) (Map.keys . Map.filter (== Nothing) $ mkBoard board)

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board co = do _ <- Map.lookup co (mkBoard board) 
                           return $ evalState (territorySearch (mkBoard board) co) newBFS

neighbourCoords :: Coord -> [Coord]
neighbourCoords (r,c) = [(x,y) | x <- [r-1, r+1], y<- [c-1, c+1] , (x,y) /= (r,c)]

territoryNeighbourCoords :: Coord -> [Coord]
territoryNeighbourCoords (r,c) = [(r, c-1), (r, c+1), (r+1, c), (r-1, c)]

filterSameColourNeighbours :: (Coord -> [Coord]) -> Board -> Coord -> Maybe [Coord]
filterSameColourNeighbours neighfn board co = do col <- Map.lookup co board
                                                 let neighbours = neighfn co
                                                 return $ [ coor | coor <- neighbours, Just col == Map.lookup coor board]
mkBoard :: [String] -> Board
mkBoard xs = Map.fromList $! go (1,1) (unlines xs) []
        where go _ [] _ = []
              go (r,_) ('\n':rest) acc = go (r+1, 1) rest acc
              go (r,c) ('B':ys) acc = ((r,c), Just Black) : go (r,c+1) ys acc
              go (r,c) ('W':ys) acc = ((r,c), Just White) : go (r,c+1) ys acc
              go (r,c) (_:ys) acc = ((r,c), Nothing) : go (r,c+1) ys acc

newBFSearch :: Coord -> State BFS ()
newBFSearch co = modify (\bfs -> bfs { queue = searchFrom })
    where searchFrom = Seq.singleton co

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

extendQueue :: Foldable t => (Coord -> [Coord]) -> Board -> t Coord -> Coord -> Reader (Seq Coord) (Seq Coord)
extendQueue neighfn board seenList co = do
    let neighbours = Set.fromList $ fromMaybe [] $ filterSameColourNeighbours neighfn board co
        unseen =  Set.filter (`notElem` seenList) neighbours
        newQueue que = Set.foldl' (Seq.|>) que unseen
    asks newQueue

capturedRegion :: Board -> Set Coord -> Maybe Color
capturedRegion board regionCoords  = let
    neighbourCols = Set.fromList $ mapMaybe (`Map.lookup` board) (concatMap neighbourCoords regionCoords)
    in eval neighbourCols
        where eval s
                | (Just Black `elem` s) == (Just White `notElem` s) = Just Black
                | (Just White `elem` s) == (Just Black `notElem` s) = Just White
                | otherwise = Nothing 

continueBFS :: Board -> State BFS (Set Coord, Maybe Color)
continueBFS board = do
    q <- gets queue
    viewed <- gets seen
    c <- gets connected
    let (res, newQueue) = runState popQueue q
    traceShowM newQueue
    case res of
        Nothing -> return (c, capturedRegion board c)
        Just co -> do let newSeen = runReader (addToSeen co) viewed
                          moreQueue = runReader (extendQueue neighbourCoords board newSeen co) newQueue
                          newConnected = runReader (addToConnected co) c
                      put $ BFS moreQueue newSeen newConnected
                      continueBFS board

territoryBFS :: Board -> State BFS (Set Coord, Maybe Color)
territoryBFS board = do
    q <- gets queue
    viewed <- gets seen
    c <- gets connected
    let (res, newQueue) = runState popQueue q
    case res of
        Nothing -> return (c, capturedRegion board c)
        Just co -> do let newSeen = runReader (addToSeen co) viewed
                          moreQueue = runReader (extendQueue territoryNeighbourCoords board viewed co) newQueue
                          newConnected = runReader (addToConnected co) c
                      put $ BFS moreQueue newSeen newConnected
                      territoryBFS board

territorySearch :: Board -> Coord -> State BFS (Set Coord, Maybe Color)
territorySearch board co = do newBFSearch co
                              territoryBFS board

connectedSearch :: Board -> Coord -> State BFS (Set Coord, Maybe Color)
connectedSearch board co = do newBFSearch co
                              continueBFS board

board5x5 :: [String]
board5x5 = [ "  B  "
            , " B B "
            , "B W B"
            , " W W "
            , "  W  " ]

board5x5B :: Board
board5x5B = mkBoard  [ "  B  "
                    , " B B "
                    , "B W B"
                    , " W W "
                    , "  W  " ]

-- | Tests the thing 
--
-- >>> test
--
test :: Int -> Int -> (Set Coord, Maybe Color)
test x y = evalState (territorySearch board5x5B (x,y)) newBFS
