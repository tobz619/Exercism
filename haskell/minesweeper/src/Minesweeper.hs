module Minesweeper (annotate) where

import Data.Either (isRight)
import Data.Char (intToDigit)

type Row = Int
type Col = Int
type Coord = (Row, Col)
data Tile = Bomb | Empty | Newline deriving (Show, Eq)

type Info = (Coord, Tile)

annotate :: [String] -> [String]
annotate [""] = [""]
annotate board = lines $ infoToString (makeInfo board) board

mineLine :: String -> [Tile]
mineLine = map parseMines
    where parseMines '*' = Bomb
          parseMines '\n' = Newline
          parseMines _ = Empty

toCoord :: [String] -> [Coord]
toCoord xs = parseChars (1,1) (unlines xs)
      where parseChars _ [] = []
            parseChars (r,c) ('\n':xs) = (r,c) : parseChars (r+1,1) xs
            parseChars (r,c) (_:xs) = (r,c) : parseChars (r,c+1) xs

makeInfo :: [String] -> [Info]
makeInfo xs =
      let info = mineLine (unlines xs)
          coord = toCoord xs
       in zip coord info

adjacents :: Coord -> [Coord]
adjacents (r,c) = [(r+1,c), (r-1, c), (r, c+1), (r, c-1),
                   (r+1,c+1) , (r-1, c+1), (r+1,c-1), (r-1,c-1)]


checkAdjacents :: Coord -> [(Coord, Tile)] -> Int
checkAdjacents (r,c) board =
      let eligCoords = filter (`elem` (adjacents (r,c))) (map fst board)
          presentTiles = filter (\((r,c),info) -> elem (r,c) eligCoords) board
       in foldr checkBomb 0 presentTiles
            where checkBomb ((_),Bomb) acc = acc + 1
                  checkBomb _ acc = acc

infoToString :: [Info] -> [String] -> String
infoToString [] board = []
infoToString (((_),Newline):xs) board = '\n': infoToString xs board 
infoToString ((_,Bomb):xs) board = '*' : infoToString xs board
infoToString (((r,c),Empty):xs) board = (case intToDigit (checkAdjacents (r,c) (makeInfo board)) of
                                          '0' -> ' '
                                          x  -> x ) : infoToString xs board