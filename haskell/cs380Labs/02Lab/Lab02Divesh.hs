-- Lab02.hs
-- Names: Divesh Otwani
--
--
-- Read the problems below.
-- When you are done, upload your work in the lab02 directory at our
-- classwork repo, at https://github.com/bmc-cs380/classwork
-- Please rename the file before upload so you don't clobber your
-- classmates' work!
--

{- OPTIONS -Wall -Wno-type-defaults #-}

module Lab02Divesh where

-- The "Prelude" is the module automatically imported into all Haskell
-- programs. But we'll be redefining some Prelude functions in this file,
-- so we don't want GHCi to get confused by having both your and the
-- Prelude's in scope. So we hide some imports.
-- (By the way, the Prelude is documented here:
--  http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html)
import Prelude hiding (all, reverse, takeWhile, zip, concat, concatMap)

import Test.HUnit

--------------------------------------------------------------------------------
-- The code in this section works -- the test passes. (Run runTestTT testStyle
-- to see this.) But the style is awful. Fix.

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , treverse ]

abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z)


tabc :: Test
tabc = "abc" ~: 
        TestList [ abc True False True  ~?= True
                 , abc True False False ~?= False
                 , abc False True True  ~?= False ]

reverse :: [a] -> [a]
reverse l = reverseAux l [] where 
                             reverseAux [] acc     = acc
                             reverseAux (x:xs) acc = reverseAux xs (x:acc)


treverse :: Test
treverse = "reverse" ~: 
           TestList [ reverse [3,2,1] ~?= [1,2,3]
                    , reverse [1]     ~?= [1] ]

--------------------------------------------------------------------------------
-- Below are many functions over lists. Write the functions and tests over those
-- functions.

testLists :: Test
testLists = "testLists" ~: TestList [ttakeWhile, tfind, tall, tmap2, tzip, ttranspose, tconcat]

-- takeWhile, applied to a predicate p and a list xs,
-- returns the longest prefix (possibly empty) of xs of elements
-- that satisfy p:
-- For example,
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a-> Bool) -> [a] -> [a]
takeWhile f []     = []
takeWhile f (x:xs) | f x       = x : (takeWhile f xs)
                   | otherwise = []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: [ takeWhile (>0) [12, 11..(-3)] ~?= [12,11..1] ]


-- find pred lst returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a "Maybe".
-- for example:
--     find odd [0,2,3,4] returns Just 3

find :: (a -> Bool) -> [a] -> (Maybe a)
find _ []     = Nothing
find f (x:xs) | f x       = Just x
              | otherwise = find f xs

tfind :: Test
tfind = "find" ~: [ find (<0) [10, 9..(-3)] ~?= Just (-1) 
                  , find (<0) [10,9..3]    ~?= Nothing ]


-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all :: (a -> Bool) -> [a] -> Bool
all f = and . map f

tall :: Test
tall = "all" ~: [ all (>0) [1..10]    ~?= True
                , all (>0) [10, 9..(-4)] ~?= False ]


-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the standard library.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
map2 _ _ _           = []


tmap2 :: Test
tmap2 = "map2" ~: [ map2 (+) [1..10] [1,3..15] ~?= zipWith (+) [1..10] [1,3..15] ]


-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:
--    zip [1,2] [True] returns [(1,True)]

pair :: a -> b -> (a,b)
pair a b = (a, b)

zip :: [a] -> [b] -> [(a,b)]
zip = map2 pair


tzip :: Test
tzip = "zip" ~: [zip [1,2] [True] ~?= [(1,True)]]


-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is not the same behavior as the library version
-- of transpose.

-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]

hasHead :: [a] -> Bool
hasHead []     = False
hashead (x:xs) = True

transpose :: [[a]] -> [[a]]
transpose xs | all (not . null) xs = (map head xs) : transpose (map tail xs)
             | otherwise      = []

ttranspose :: Test
ttranspose = "transpose" ~: [ transpose [[1,2,3], [4,5,6]] ~?= [[1,4],[2,5],[3,6]]
                            ]


-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]

concat :: [[a]] -> [a]
concat = foldl (++) []

tconcat :: Test
tconcat = "concat" ~: [ concat ["this", " is", " a", " test"] ~?= "this is a test" ]


-- concatMap

-- Map a function over all the elements of the list and concatenate the results.
-- for example:
--    concatMap (\x -> [x,x+1,x+2]) [1,2,3]  returns [1,2,3,2,3,4,3,4,5]

concatMap :: (a -> [a]) -> [a] -> [a]
concatMap f = concat . map f

tconcatMap :: Test
tconcatMap = "concatMap" ~: [ concatMap (\x -> [x,x+1,x+2]) [1,2,3] 
                                ~?= [1,2,3,2,3,4,3,4,5] ]










