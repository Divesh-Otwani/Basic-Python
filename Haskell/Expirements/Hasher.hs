module Hasher where


import Data.Char (ord)

hash :: String -> Int
hash xs = mod (sum $ map convert xs) 10 where
  convert x = (ord x) - 97


instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = xs
  [] + ys = ys
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []

-- taken from online
choose :: Int -> Int -> Int
choose n k = ([1,1]^n) !! k

c = choose 7

fixonepath :: Int -> Int
fixonepath i = (c i')*( (c i'') + 2*(c i') + (c i) ) where
  i' = i-1
  i'' = i-2


sumList [] = 0
sumList (x:xs) = x + sumList xs

middle = sumList $ map fixonepath [2..7]





