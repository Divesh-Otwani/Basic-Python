{-# LANGUAGE GADTs #-}
module Game where

import Graphics.Gloss ( play, Display(..) )
import Graphics.Gloss.Data.Color ( white )
import Graphics.Gloss.Data.Picture (Picture, circle, text, translate, scale, polygon, pictures)
import Graphics.Gloss.Interface.IO.Game (Event(..))


-- Get the screen size
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Display as D
import System.IO.Unsafe ( unsafePerformIO )

main :: IO ()
main = play disp white numStepPerSec start makePic eventHandler stepFn

numStepPerSec :: Int
numStepPerSec = 10


---------------------------------------------------------------------
-- Get dims of screen
getheight :: IO Int
getheight = do
  d <- X.openDisplay ""
  return $ fromIntegral $ D.displayHeight d (D.defaultScreen d)


getwidth :: IO Int
getwidth = do
  d <- X.openDisplay ""
  return $ fromIntegral $ D.displayWidth d (D.defaultScreen d)

{-# NOINLINE height #-}
height :: Int
height = unsafePerformIO getheight

{-# NOINLINE width #-}
width :: Int
width = unsafePerformIO getwidth
-----------------------------------------------------------------------------

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
disp = InWindow "Title here" (width, height) (0,0)

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








