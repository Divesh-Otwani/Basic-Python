module Harmonic where

main :: IO ()
main = do
  putStrLn "Printing harmonic sums:"
  loop (1,1)

loop :: (Int,Double) -> IO ()
loop (n,sum) = do
  let next = harmonic sum n
  case (mod n 10000000 == 0) of
    True -> do putStrLn $ show $ next
    False -> return ()
  loop (n+1, next)


-- get's nth sum of series
-- assume input is a nat

harmonic :: Double -> Int -> Double
harmonic sum n = (1.0 / (fromIntegral n)) + sum





