module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = concat [makeB (x, y) | x <- [0 .. 7], y <- [0 .. 7]]
                    where makeB (x,y)
                            | Just (x,y) == white && y == 7 = "W\n"
                            | Just (x,y) == black && y == 7 = "B\n"
                            | Just (x,y) == white = "W "
                            | Just (x,y) == black = "B "
                            | y == 7 = "_\n"
                            | otherwise = "_ "

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = or [ sameRow queenA queenB
                             , sameCol queenA queenB
                             , sameDiag queenA queenB
                             , sameAntidiag queenA queenB ]

sameRow :: (Int, Int) -> (Int, Int) -> Bool
sameRow (i,_) (k,_) = i == k 

sameCol :: (Int, Int) -> (Int, Int) -> Bool
sameCol (_,j) (_,l) = j == l

sameDiag :: (Int, Int) -> (Int, Int) -> Bool
sameDiag (i,j) (k,l) = (i-k) == (j-l)

sameAntidiag :: (Int, Int) -> (Int, Int) -> Bool
sameAntidiag (i,j) (k,l) = (i-k) == (l-j)

