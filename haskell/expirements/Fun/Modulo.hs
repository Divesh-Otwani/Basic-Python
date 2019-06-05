module Modulo where


wierdNameThing :: Int -> Int
wierdNameThing = (+ 1)


data Mod2 = Zero | One
  deriving (Eq, Show)

instance Num Mod2 where
  Zero + x = x
  x + Zero = x
  One + One = Zero

  negate = id
  fromInteger n = case (n `mod` 2) of
    1 -> One
    0 -> Zero
    _ -> error "impossible"

  abs = id
  Zero * _ = Zero
  _ * Zero = Zero
  _ * _       = One


stupid :: Int -> Int -> Int

stupid 1 _ = 1
stupid 3 _ = 3
stupid x y    = x + y








