module Spiral (spiral) where

{- For this one, I was thinking that I could make a spiral type 
   that told me when turns were happening. 
   
   Since the list will always
   be a square, it was a case of figuring out the type and making a from list function
   that put the turns in the right place.  
   
   Then a new problem: I must somehow now make these into lists that are somehow 
   appending in seeming non-standard orders :: I had to keep track of direction and position.  
   
   This became easier once I put it into a Coordinate map (I really should use Data.Map) 
   And created a type I could use to keep track of direction.  
   
   Unfoldr is great because it keeps track of state and will continue until the list is 
   empty. Now with the direction, just change the axis by the correct amount :) -}

import Data.List (unfoldr, groupBy, sortBy)

data Spiral a = Turn a (Spiral a) | Forward a (Spiral a) | End deriving Show

data Coord = Coord {row :: Int, column :: Int} deriving Show

data Dir = R | D | L | U deriving (Show, Enum)

{-| Allows us to cycle our directions -}
next :: Dir -> Dir
next U = R
next x = succ x

spiral :: Int -> [[Int]]
spiral = (fmap.fmap) snd . toRows . spiralBuilder . spiralList

{-| Sorts the rows and columns into the right order after spiralBuilder -}
toRows :: [(Coord, b)] -> [[(Coord, b)]]
toRows = groupBy (\x y -> (row . fst) x == (row . fst) y) .
         sortBy (\x y -> (row . fst) x `compare` (row . fst) y) . 
         sortBy (\x y -> (column . fst) x `compare` (column . fst) y)

{-| Uses the direction and previous coordinate to insert the item 
    at the correct coordinate and returns the new coord with the item there -}
insert :: Dir -> b -> Coord -> (Coord, b)
insert R x c = (c {column = column c + 1}, x)
insert D x c = (c {row = row c + 1}, x)
insert L x c = (c {column = column c - 1}, x)
insert U x c = (c {row = row c - 1}, x)


{-| Uses a triple to keep track of where we are in the matrix and provide the values 
    for each step of the build process -}
spiralAux :: (Coord, Dir, [[a]]) -> Maybe ((Coord, a), (Coord, Dir, [[a]]))
spiralAux (_, _, []) = Nothing
spiralAux (c, d, []:xs) = spiralAux (c, next d ,xs)
spiralAux (c, d,(x:xs):ys) = Just (inserted,seed)
                            where inserted = insert d x c
                                  seed = (fst inserted, d,xs:ys)


{-| Allows us to start a spiral from our list of values in direction order -}
spiralBuilder :: [[a]] -> [(Coord, a)]
spiralBuilder xs = unfoldr spiralAux (Coord 0 1, R, xs)

spiralMaker :: Int -> Spiral Int
spiralMaker n = fromList n 1 [1..n^2]

{-| A function that calculates where turns are supposed to be -}
fromList :: Int -> Int -> [a] -> Spiral a
fromList _ _ [] = End
fromList n t xs = go n t (n-1) xs
        where go _ _ _ [] = End
              go n 0 n' xs = fromList (n-1) 2 xs
              go n t 0 (x:xs)  = Turn x (go n (t-1) (n-1) xs)
              go n t n' (x:xs) = Forward x (go n t (n' - 1) xs)



{-| Takes our spiral structure and generates a list of lists in direction order:
    Each turn is the next list in the list: The order being R-D-U-L -}
spiralToLists :: Spiral a -> [[a]]
spiralToLists = go []
           where go acc End = pure . reverse $ acc
                 go acc (Turn a End) = pure . reverse $ (a : acc)
                 go acc (Turn a b) = reverse (a : acc) : spiralToLists b
                 go acc (Forward a b) = go (a:acc) b

{-| Shortcut to allow us to get the list of lists from a single int-}
spiralList :: Int -> [[Int]]
spiralList = spiralToLists . spiralMaker