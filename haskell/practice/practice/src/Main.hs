{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.Function ((&))
import Text.Printf (printf)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Char
import Data.List hiding (insert, delete)
import Data.List.Split


-- # Current Problem
-------------------------------------------------------------------------------





-- # Main
-------------------------------------------------------------------------------


main :: IO ()
main = return ()


-- # Reference
-------------------------------------------------------------------------------

-- Given natural number input
prime_factors :: Integral i => i -> [(i, Int)]
prime_factors n = loop n [] where
  loop :: Integral i => i -> [(i, Int)] -> [(i, Int)]
  loop 1 !acc = acc
  loop n !acc = find (\p -> n `mod` p == 0) primes &
    \(Just prime_divisor) -> maxPower prime_divisor n &
      \prime_power ->
        let !acc' = (prime_divisor, prime_power) : acc in
        loop (n `div` (prime_divisor ^ prime_power)) acc'

maxPower :: Integral i => i -> i -> Int
maxPower prime k | (!q,!r) <- divMod k prime = case r of
  0 -> 1 + maxPower prime q
  _ -> 0

-- Returns input if it is prime
nextPrime :: Integral i => i -> i
nextPrime i = head $ dropWhile (< i) primes



primesBelow :: Integral i => i -> [i]
primesBelow i = takeWhile (< i) primes

-- https://stackoverflow.com/questions/3596502/lazy-list-of-prime-numbers
primes :: Integral i => [i]
primes = 2: 3: sieve (tail primes) [5,7..]
 where
  sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
                                -- or:  filter ((/=0).(`rem`p)) t
                  where (h,~(_:t)) = span (< p*p) xs


isPrime :: Integer -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = let potentialdivisors = pos_divisors x in
                  null [k | k <- potentialdivisors, mod x k == 0]

pos_divisors :: Integer -> [Integer]
pos_divisors n = [2..floor (sqrt $ fromIntegral n)]






{-

generateHashtag :: String -> Maybe String
generateHashtag = fmap ('#':) . clean

clean :: String -> Maybe String
clean =
  tooLong .
  concat .
  map (\(x:xs) -> toUpper x : xs) .
  words

tooLong s
  | length s > 140 = Nothing
  | otherwise = Just s




maxPower :: Int -> Int -> Int
maxPower prime k | (!q,!r) <- divMod k prime = case r of
  0 -> 1 + maxPower prime q
  _ -> 0

freqLetters :: String -> M.Map Char Int
freqLetters = foldl' (flip $ M.alter incInt) M.empty where

  incInt :: Maybe Int -> Maybe Int
  incInt Nothing = Just 1
  incInt (Just n) = Just (n+1)



rev_digits :: Integer -> [Integer]
rev_digits 0 = [0]
rev_digits n = rev_digits' . abs $ n where
  rev_digits' 0 = []
  rev_digits' n | (q,r) <- divMod n 10 = r : rev_digits' q


permutations :: String -> [String]
permutations = noDup . sort . withPref ""

noDup :: Eq a => [a] -> [a]
noDup [] = []
noDup (x:[]) = [x]
noDup (x:y:xs)
  | x == y = noDup (y:xs)
  | otherwise = x : noDup (y:xs)

withPref :: [a] -> [a] -> [[a]]
withPref p [] = [p]
withPref p (x:xs) =
  concatMap (\p' -> withPref p' xs) (allInserts x p)

allInserts :: a -> [a] -> [[a]]
allInserts x [] = [[x]]
allInserts x (y:ys) = (x:y:ys) : (map (y:) $ allInserts x ys)


-- n is lenght of an array
pairs :: Int -> [(Int,Int)]
pairs n = [(i,j) | i <- [0..(n-1)], j <- [i..(n-1)]]

-- For natural numbers
digits :: Int -> [Int]
digits = reverse . rev_digits

-- For natural numbers
rev_digits :: Int -> [Int]
rev_digits 0 = []
rev_digits n | (q,r) <- divMod n 10 = r : rev_digits q



-- lazy and too long
rotate :: Int -> Int -> [Int] -> [Int]
rotate n k xs | k' <- mod k n = drop (n-k') xs ++ xs

-- Called with naturals
my_gcd :: Integer -> Integer -> Integer
my_gcd a b
  | a <= b = gcdLeq a b
  | otherwise = gcdLeq b a
  where
    gcdLeq :: Integer -> Integer -> Integer
    gcdLeq !a !b | (!q,!r) <- divMod b a = case r of
      0 -> a
      _ -> gcdLeq r a


factors' :: Integer -> [Integer]
factors' n = [k | k <- [1..n], n `mod` k == 0]

interweave :: [a] -> [a] -> [a]
interweave [] ys = ys
interweave xs [] = xs
interweave (x:xs) (y:ys) = x : y : interweave xs ys


-- Called with natural > 0
digit_sum :: Integer -> Integer
digit_sum = loop 0 where
  loop :: Integer -> Integer -> Integer
  loop !acc 0 = acc
  loop !acc n | (q,r) <- divMod n 10 = loop (r+acc) q

factors :: Integer -> [Integer]
factors = factors_pfac . prime_factors

factors_pfac :: [(Integer, Int)] -> [Integer]
factors_pfac xs = do
  let (ps, es) = unzip xs
  !powers <- mapM choosePower es
  let !fac = product $ zipWith (\p e -> p ^ e) ps powers
  guard (fac /= 0)
  return fac

choosePower :: Int -> [Int]
choosePower e = do
  pow <- [0..e]
  return pow

-- Given a natural n
prime_factors :: Integer -> [(Integer, Int)]
prime_factors n = loop n [] where
  loop :: Integer -> [(Integer, Int)] -> [(Integer, Int)]
  loop 1 !acc = acc
  loop n !acc = find (\p -> n `mod` p == 0) primes &
    \(Just prime_divisor) -> maxPower prime_divisor n &
      \prime_power ->
        let !acc' = (prime_divisor, prime_power) : acc in
        loop (n `div` (prime_divisor ^ prime_power)) acc'


maxPower :: Integer -> Integer -> Int
maxPower prime k | (!q,!r) <- divMod k prime = case r of
  0 -> 1 + maxPower prime q
  _ -> 0

-- https://stackoverflow.com/questions/3596502/lazy-list-of-prime-numbers
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
 where
  sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
                                -- or:  filter ((/=0).(`rem`p)) t
                  where (h,~(_:t)) = span (< p*p) xs

-}
