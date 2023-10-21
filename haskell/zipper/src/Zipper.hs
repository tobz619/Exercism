module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

import Data.Maybe (fromMaybe, isNothing)

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Direction a = L (BinTree a) 
                 | R (BinTree a) 
                 deriving (Show, Eq)

type Zipper a = ([Direction a], BinTree a)

fromTree :: BinTree a -> Zipper a
fromTree t = ([],t)


toTree :: Zipper a -> BinTree a
toTree zipper@(_,t) = maybe t toTree (up zipper)

value :: Zipper a -> a
value (_,t) = btValue t

left :: Zipper a -> Maybe (Zipper a)
left (ds, t) = btLeft t >>= \tree ->
               Just (L t:ds, tree)

right :: Zipper a -> Maybe (Zipper a)
right (ds, t) = btRight t >>= \tree ->
                Just (R t:ds, tree)

up :: Zipper a -> Maybe (Zipper a)
up ([],_) = Nothing
up (L orig:ds, t) = Just (ds, orig {btLeft = Just t})
up (R orig:ds, t) = Just (ds, orig {btRight = Just t})


setValue :: a -> Zipper a -> Zipper a
setValue x zipper@(dirs, tree) = (dirs, tree {btValue = x})

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft t zipper@(dirs, tree) = (dirs, tree {btLeft = t})

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight t zipper@(dirs, tree) = (dirs, tree {btRight = t})


testtree :: BinTree Integer
testtree = BT 3
           (Just (BT 4
                 (Just (BT 6
                        (Just (BT 9009
                                  Nothing
                                  Nothing
                              )
                        )
                        (Just (BT 4905
                                   Nothing
                                   Nothing
                         ))
                        )
                 ) (Just (BT 369
                                   (Just (BT 123
                                             Nothing
                                             Nothing
                         ))
                                   Nothing
                         ))
                 )
           )
           (Just (BT 5
                  Nothing
                  (Just (BT 52025
                            Nothing
                            Nothing
                         ))
                 )
           )