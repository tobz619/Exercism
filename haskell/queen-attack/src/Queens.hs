module Queens (boardString, canAttack) where

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