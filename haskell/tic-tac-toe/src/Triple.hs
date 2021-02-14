module Triple (T(..), Tix(..), (!:), update) where

newtype T a = T (a,a,a) deriving (Show, Eq)

data Tix = T1 | T2 | T3 deriving (Show, Eq, Enum)

(!:) :: T a -> Tix -> a
(T (a,b,c)) !: tix = case tix of
  T1 -> a
  T2 -> b
  T3 -> c

update :: T a -> Tix -> a -> T a
update (T (a,b,c)) tix x = case tix of
  T1 -> T (x,b,c)
  T2 -> T (a,x,c)
  T3 -> T (a,b,x)
