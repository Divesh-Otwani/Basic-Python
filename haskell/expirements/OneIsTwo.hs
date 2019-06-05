{-# LANGUAGE TypeFamilies, TypeInType,
    TypeOperators,
    GADTs, RecordWildCards, StandaloneDeriving #-}
module OneIsTwo where

import Data.Kind ( Type )

data Nat where
  Z :: Nat
  Succ :: Nat -> Nat
  deriving Show

data SNat :: Nat -> Type where
  SZ :: SNat Z
  SSucc :: SNat n -> SNat (Succ n)

deriving instance Show (SNat n)


{-
type family Loop :: Type where
  Loop = [Loop]


data ExRefl where
  ExRefl :: a -> a -> ExRefl

-}

data a :~: b where
  Refl :: a :~: a


instance Show (a :~: b) where
  show Refl = "Refl"


{-
proof :: SNat Z -> SNat (Succ Z) -> Z :~: (Succ Z)
proof x y = proof x y
-}

proof' :: Z :~: (Succ Z)
proof' = proof'





