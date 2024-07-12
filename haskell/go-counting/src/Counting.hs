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
import Data.Maybe (mapMaybe, isNothing)
import Control.Monad.State
import Control.Monad.Reader

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
territories board = Set.toList $ Set.fromList $ mapMaybe (territoryFor board) (Map.keys . Map.filter isNothing $ mkBoard board)

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board co = do c <- Map.lookup co (mkBoard board)
                           case c of
                             Nothing -> return $ evalState (territorySearch (mkBoard board) co) newBFS
                             _ -> Nothing

libertyCoords :: Coord -> [Coord]
libertyCoords (x,y) = [(x, y-1), (x, y+1), (x+1, y), (x-1, y)]

growEmpty :: (Coord -> [Coord]) -> Board -> Coord -> [Coord]
growEmpty neighfn board co = [coords | coords <- neighfn co, Just Nothing == Map.lookup coords board]

mkBoard :: [String] -> Board
mkBoard xs = Map.fromList $! go (1,1) (unlines xs) []
        where go _ [] _ = []
              go (_,y) ('\n':rest) acc = go (1,y+1) rest acc
              go (x,y) ('B':ys) acc = ((x,y), Just Black) : go (x+1,y) ys acc
              go (x,y) ('W':ys) acc = ((x,y), Just White) : go (x+1,y) ys acc
              go (x,y) (_:ys) acc = ((x,y), Nothing) : go (x+1, y) ys acc

newBFSearch :: Coord -> State BFS ()
newBFSearch co = modify (\bfs -> bfs { queue = Seq.singleton co })


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
    let neighbours = Set.fromList $ growEmpty neighfn board co
        unseen =  Set.filter (`notElem` seenList) neighbours
        newQueue que = Set.foldl' (Seq.|>) que unseen
    asks newQueue

capturedRegion :: Board -> Set Coord -> Maybe Color
capturedRegion board regionCoords  = let
    neighbourCols =  mapMaybe (`Map.lookup` board) (concatMap libertyCoords regionCoords)
    in eval neighbourCols
        where eval s
                | (Just Black `elem` s) && (Just White `notElem` s) = Just Black
                | (Just White `elem` s) && (Just Black `notElem` s) = Just White
                | otherwise = Nothing

territoryBFS :: Board -> State BFS (Set Coord, Maybe Color)
territoryBFS board = do
    q <- gets queue
    viewed <- gets seen
    c <- gets connected
    let (res, newQueue) = runState popQueue q
    case res of
        Nothing -> return (c, capturedRegion board c)
        Just co -> do let newSeen = runReader (addToSeen co) viewed
                          moreQueue = runReader (extendQueue libertyCoords board viewed co) newQueue
                          newConnected = runReader (addToConnected co) c
                      put $ BFS moreQueue newSeen newConnected
                      territoryBFS board

territorySearch :: Board -> Coord -> State BFS (Set Coord, Maybe Color)
territorySearch board co = do newBFSearch co
                              territoryBFS board

board5x5 :: [String]
board5x5 =  [ "  B  "
            , " B B "
            , "B W B"
            , " W W "
            , "  W  " ]

board5x5B :: Board
board5x5B = mkBoard  board5x5
