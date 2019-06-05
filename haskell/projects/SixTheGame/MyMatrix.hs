{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module MyMatrix
  ( Matrix
  , maybeIndexM
  , maybeSetM
  , queryDims
  , prettyPrint
  ) where

-- This is massively slow, but adaptable

-- Say, list of cols
type Matrix a = [[a]]


(!>!) :: [a] -> Int -> Maybe a
[]     !>! _ = Nothing
(x:xs) !>! n = case compare n 0 of
  EQ -> Just x
  LT -> Nothing
  GT -> xs !>! (n-1)


index :: [a] -> Int -> Maybe a
index = (!>!)

-- (row, col)
maybeIndexM :: (Int, Int) -> Matrix a -> Maybe a
maybeIndexM (row, col) mtx =
  case (row < 0 || col < 0) of
    True -> Nothing
    False -> do
      theCol <- index mtx col
      cell <- index theCol row
      return cell


maybeSetM :: (Int, Int) -> a -> Matrix a -> Maybe (Matrix a)
maybeSetM (row, col) newVal m = do
  theCol <- index m col
  newCol <- maybeSetList row newVal theCol
  maybeSetList col newCol m



maybeSetList :: Int -> a -> [a] -> Maybe [a]
maybeSetList _ _ [] = Nothing
maybeSetList 0 a (_:xs) = Just $ a:xs
maybeSetList n a (x:xs) | n <= 0    = Nothing
                        | otherwise = do
                            recur <- maybeSetList (n-1) a xs
                            return $ x : recur


xprettyPrint :: Show a => Matrix a -> String
xprettyPrint [] = ""
xprettyPrint (x:xs) = show x ++ "\n" ++ xprettyPrint xs

prettyPrint :: Show a => Matrix a -> IO ()
prettyPrint m = putStrLn $ xprettyPrint m


queryDims :: Matrix a -> (Int, Int)
queryDims [] = (0,0)
queryDims l@(x:_) = (length x, length l)







