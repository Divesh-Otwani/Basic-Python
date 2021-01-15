module Display
  ( eventHandler
  , displayGame
  ) where

import Data.Maybe (mapMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact

import Triple
import Board
import Game

-- # API
-------------------------------------------------------------------------------

eventHandler :: Event -> GameState -> GameState
eventHandler _ s@(Winner _ _) = s
eventHandler e (Ongoing p b) = maybe (Ongoing p b) id $ do
  point <- getClickPoint e
  spot <- getValidSpot point
  nextBoard <- move p spot b
  return $ checkIfWon p nextBoard

getClickPoint :: Event -> Maybe (Float, Float)
getClickPoint (EventKey (MouseButton LeftButton) Down _ spot) = Just spot
getClickPoint _ = Nothing

getValidSpot :: (Float, Float) -> Maybe (Tix, Tix)
getValidSpot (x,y)
  | -300 <= x && x <= 300 && -300 <= y && y <= 300 =
      let {x' = floor x; y' = floor y} in
        Just (toEnum $ (x'+300) `div` 200, toEnum $ (300-y') `div` 200)
  | otherwise = Nothing

checkIfWon :: Player -> Board -> GameState
checkIfWon lastPlayed b =
  if or $ mapMaybe (checkList b) allPlaces
  then Winner lastPlayed b
  else Ongoing (otherPlayer lastPlayed) b
  where

    ts = [T1, T2, T3]
    row i = [(i,j) | j <- ts]
    col j = [(i,j) | i <- ts]
    rows = [row i | i <- ts]
    cols = [col i | i <- ts]
    d1 = [(T1,T1), (T2,T2), (T3,T3)]
    d2 = [(T3,T1), (T2,T2), (T1,T3)]
    allPlaces = d1 : d2 : (rows ++ cols)

    checkList :: Board -> [(Tix, Tix)] -> Maybe Bool
    checkList b (x:y:z:[]) = do
       m1 <- ixBoard b x
       m2 <- ixBoard b y
       m3 <- ixBoard b z
       return $ m1 == m2 && m2 == m3
    checkList _ _ = Nothing


displayGame :: GameState -> Picture
displayGame (Winner p b) = pictures
  [ displayBoard b
  , translate (-150) (-360) $ scale 0.3 0.3 $ text ("Winner: " ++ show p)
  ]
displayGame (Ongoing p b) = pictures [toPlay, board] where
  toPlay = translate (-150) (-360) $ scale 0.3 0.3 $ text ("To play " ++ show p)
  board = displayBoard b


-- # Displaying a board
-------------------------------------------------------------------------------

displayBoard :: Board -> Picture
displayBoard b = pictures $
  frame : mapMaybe (displayIx b) [(i,j) | i <- ts, j <- ts]
  where
    ts = [T1, T2, T3]
    frame = pictures [rectangleWire 600 600, l1, l2, l3, l4]
    l1 = line [(-100,300), (-100, -300)]
    l2 = line [(100,300), (100, -300)]
    l3 = line [(-300, 100), (300, 100)]
    l4 = line [(-300, -100), (300, -100)]

    displayIx :: Board -> (Tix, Tix) -> Maybe Picture
    displayIx board spot = do
      mark <- ixBoard board spot
      case mark of
        Xmark -> return $ displayX (toFloats $ centerIx spot)
        Omark -> return $ displayO (toFloats $ centerIx spot)

    toFloats :: (Int, Int) -> (Float, Float)
    toFloats (x,y) = (fromIntegral x, fromIntegral y)

    centerIx :: (Tix, Tix) -> (Int, Int)
    centerIx (r,c) = ((fromEnum r)*200 - 200, 200 - (fromEnum c)*200)

    displayX :: (Float, Float) -> Picture
    displayX (x,y) = pictures [ line [(x+100,y+100), (x-100,y-100)]
                              , line [(x+100,y-100), (x-100,y+100)]
                              ]

    displayO :: (Float, Float) -> Picture
    displayO (x,y) = translate x y (circle 100)

