module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

instance Functor BST where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node x l _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node x _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue  Empty = Nothing
bstValue (Node x _ _) = Just x

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty . reverse

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node y l r)
 | x > y = Node y l (insert x r)
 | x <= y = Node y (insert x l) r
 | otherwise = Node y l r

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node x l r) =  toList l ++ [x] ++ toList r