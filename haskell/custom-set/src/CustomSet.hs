module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

import qualified Data.List as L

data CustomSet a = Nil | Elem a (CustomSet a)

instance Eq a => Eq (CustomSet a) where
  Nil == Nil = True
  Elem a l == Elem b r = if a == b then l == r else False
  _ == _ = False

instance Show a => Show (CustomSet a) where
  show x = '{' : go x
          where go Nil = "}"
                go (Elem x Nil) = show x ++ "}"
                go (Elem x a) = show x ++ "," ++ go a 

ex :: CustomSet Int
ex = Elem 2 (Elem 3 (Elem 4 Nil))

delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete _ Nil = Nil
delete a (Elem x xs)
 | a == x = xs
 | otherwise = Elem x (delete a xs)

difference :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = let (listA, listB) = (toList setA, toList setB)
                           diffs a acc
                              | a `elem` listB = acc
                              | otherwise = a : acc
                        in fromList $ foldr diffs [] listA
                                        
empty :: CustomSet a
empty = Nil

fromList :: (Ord a) => [a] -> CustomSet a
fromList [] = Nil
fromList xs = foldr insert Nil xs 

insert :: (Ord a, Eq a) => a -> CustomSet a -> CustomSet a
insert x Nil = Elem x Nil
insert x (Elem a as)
 | x < a = Elem x (insert a as)
 | x > a = Elem a (insert x as)
 | otherwise = Elem a as

intersection :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = let (la, lb) = (toList setA, toList setB)
                          in fromList [x | x <- la , x <- lb, elem x la && elem x lb]

isDisjointFrom :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = f $ intersection setA setB
                        where f Nil = True
                              f _ = False

isSubsetOf :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf Nil _ = True
isSubsetOf _ Nil = False 
isSubsetOf setA setB = foldr (allMembs setB) True (toList setA)
                        where allMembs _ _ False = False
                              allMembs set x acc = member x set

member :: (Eq a) => a -> CustomSet a -> Bool
member x  = (x `elem`) . toList

null :: CustomSet a -> Bool
null Nil = True
null _ = False

size :: CustomSet a -> Int
size els = go 0 els
        where go c (Elem a as) = go (c+1) as
              go c Nil = c

toList :: CustomSet a -> [a]
toList Nil = []
toList (Elem x xs) = x : toList xs 

union :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = foldr insert Nil [ x | x <- toList setA ++ toList setB]

a, b, c, d :: CustomSet Integer
a = fromList [1..5]
b = fromList [6..10]
c = fromList [3..7]
d = fromList [8..12]