{-# LANGUAGE GADTSyntax #-}
module Lab03Divesh where

import Prelude hiding (lookup)

-- See http://www.seas.upenn.edu/~cis552/15fa/lectures/stub/SecretCode.html perhaps
-- wilmington exercises?

-- Lab03.hs
-- Names: Divesh Otwani
--
--
-- Read the problems below.
-- When you are done, upload your work in the lab03 directory at our
-- classwork repo, at https://github.com/bmc-cs380/classwork
-- Please rename the file before upload so you don't clobber your
-- classmates' work!
{- 1. Peer Review

   Taking turns, each member of your group should seek peer review about their
   solutions to hw01. Although scrutiny of any of hw01 is a good idea, make
   sure to discuss at least these functions:
     - maximumTableArea
     - containsStr
     - merge
     - mergeSorted

   Compare different styles and approaches to the problems. How did the others
   figure out the answers? Do any of you have lingering questions?

   During this process, remember that everyone in this class has a different
   level of experience and be supportive of one anothers' challenges. This is
   not a time to show off!

   I expect this will take at least 20 minutes to get through everyone's work.
-}

{- 2. List exercises

   Complete the List comprehension exercises from our syllabus page on the
   website (in the row for class 4).

   If you're having fun here, look up Project Euler problem #4, which is
   also solvable using a list comprehension.

-}

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

prob1 = sum [x | x <- [1..1000], (mod x 3) == 0 || (mod x 5) == 0]
prob2 = sum [x | x <- (takeWhile (<4000000) fibs), even x]

digits :: Integral t => t -> [t]
digits 0 = []
digits x = (mod x 10) : digits (div x 10)


ispal :: (Eq a) => [a] -> Bool
ispal xs = xs == (reverse xs)

prob4 = maximum [x*y | x <- [100..999], y <- [100..999], ispal (digits (x*y))]
    -- this hangs a bit.

{- 3. Library functions

   Look up the following library functions on Hoogle to find their types and
   definitions. Then, reimplement them yourself.

     - mapMaybe
     - find
     - lookup
     - maybeToList
     - concatMap

-}

isNothing :: Maybe a -> Bool
isNothing Nothing = False
isNothing _       = True

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = map (\(Just x) -> x) . (filter isNothing) . (map f)

find :: (a -> Bool) -> [a] -> Maybe a
find _ []     = Nothing
find f (x:xs) | f x       = Just x
              | otherwise = find f xs

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup _ []               = Nothing
lookup key ((k,v):xs) | key==k    = Just v
                      | otherwise = lookup key xs

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]


concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f


{- 4. Binary trees -}

-- Here is a binary tree type:
data Tree a where
  Leaf :: Tree a
  Node :: a      -- data
       -> Tree a -- left child
       -> Tree a -- right child
       -> Tree a
  deriving (Eq, Show)

-- Write a function that finds if an element is within a given tree:
elemTree :: Eq a => a -> Tree a -> Bool
elemTree _ Leaf                  = False
elemTree a (Node val left right) = a==val || elemTree a left || elemTree a right

-- Write a function that swaps the elements in a tuple in a tree.
-- Note that this does not swap left children with right children.
swapTree :: Tree (a,b) -> Tree (b,a)
swapTree Leaf                    = Leaf
swapTree (Node (a,b) left right) = Node (b,a) (swapTree left) (swapTree right)

-- Write a function that computes the depth of the tree: that is,
-- the largest number of Nodes to traverse on the way to a Leaf.
depth :: Tree a -> Int
depth Leaf                = 0
depth (Node _ left right) = (+) 1  (max (depth left) (depth right))






