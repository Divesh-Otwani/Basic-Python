module Triple (T(..), Tix(..), tix, (!:), update, checkSame) where

newtype T a = T (a,a,a) deriving (Show, Eq)

data Tix = T1 | T2 | T3 deriving (Show, Eq, Enum)

tix :: T a -> Tix -> a
tix (T (a,b,c)) tix = case tix of
  T1 -> a
  T2 -> b
  T3 -> c

(!:) :: T a -> Tix -> a
ta !: ix = tix ta ix

checkSame :: Eq a => T a -> Bool
checkSame (T (x,y,z)) = x == y && y == z

update :: T a -> Tix -> a -> T a
update (T (a,b,c)) tix x = case tix of
  T1 -> T (x,b,c)
  T2 -> T (a,x,c)
  T3 -> T (a,b,x)
