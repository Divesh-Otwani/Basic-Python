module Heman where

data BST a = Leaf a | Node a (BST a) (BST a)


insert :: (Ord a) => [a] -> BST a
insert = undefined

toList :: BST a -> [a]
toList = undefined

treeSort :: Ord a => [a] -> [a]
treeSort = toList . insert




