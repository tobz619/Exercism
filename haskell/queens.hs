module Queens (boardString, canAttack) where
import Data.Char (isSpace)



empty = boardString Nothing Nothing

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = concat [makeB (x, y) | x <- [0 .. 7], y <- [0 .. 7]]
                    where makeB (x,y)
                            | Just (x,y) == white = "W "
                            | Just (x,y) == black = "B "
                            | otherwise = "_ "

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB
 | or [ sameRow queenA queenB
             , sameCol queenA queenB
             , sameDiag queenA queenB
             , sameAntidiag queenA queenB ] = True
 | otherwise = False

sameRow :: (Int, Int) -> (Int, Int) -> Bool
sameRow (i,j) (k,l)
 | i == k = True
 | otherwise = False

sameCol :: (Int, Int) -> (Int, Int) -> Bool
sameCol (i,j) (k,l)
 | j == l = True
 | otherwise = False

sameDiag :: (Int, Int) -> (Int, Int) -> Bool
sameDiag (i,j) (k,l)
 | (i-k) == (j-l) = True
 | otherwise = False

sameAntidiag :: (Int, Int) -> (Int, Int) -> Bool
sameAntidiag (i,j) (k,l)
 | (i-k) == (l-j) = True
 | otherwise = False


boardString' :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString' white black = unlines
    [ unwords [ board i j | j <- [0 .. 7] ] | i <- [0 .. 7] ]
  where
    board i j | Just (i, j) == white = "W"
              | Just (i, j) == black = "B"
              | otherwise            = "_"