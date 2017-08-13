{-# LANGUAGE GADTs #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color ( white )
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

width = 1400
height = 900

main :: IO ()
main =
  play
    disp
    white
    numStepPerSec
    start
    makePic
    eventHandler
    stepFn

numStepPerSec :: Int
numStepPerSec = 10

data MyCircle where
  MyCircle ::
    Float ->
    (Float, Float) ->
    MyCircle

start :: MyCircle
start = MyCircle 50 (0, 0)

makePic :: MyCircle -> Picture
makePic (MyCircle r (x,y)) =
  pictures pics where
  pics =
    [
      translate x y $ circle r,
       translate x y $ scale 0.1 0.1 $
      text $
        "r=" ++
        show r ++
        " (x,y)=" ++
        "(" ++
        show x ++
        "," ++
        show y ++
        ")"
    ]

eventHandler :: Event -> MyCircle -> MyCircle
eventHandler (EventMotion x@(a, b)) _ =
  MyCircle radius x where
    r :: Float
    r = sqrt (a*a + b*b)
    radius :: Float
    radius = upperBound 200 $ (10000 / r) + 10
eventHandler _ x = x

upperBound :: Float -> Float -> Float
upperBound bound n | n >= bound    = bound
                   | otherwise = n


stepFn :: Float -> MyCircle -> MyCircle
stepFn _ = id


--  animate fs white anim2
--  display disp white pic

anim2 :: Float -> Picture
anim2 time = circle $ 8 * time

anim :: Float -> Picture
anim x = circle $ fromIntegral radius where
  round :: Int
  round = truncate x
  scale = round * round * 7
  radius :: Int
  radius = mod scale 300



disp :: Display
disp = InWindow "Title here" (width, height) (0, 0)

fs :: Display
fs = FullScreen



p1 = (10, 10)
p2 = (50, 40)
p3 = (25, 70)

triangle = [p1, p2, p3]

pic :: Picture
pic = polygon triangle

{-

Design.





-}








