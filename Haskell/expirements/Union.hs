{-# LANGUAGE TypeFamilies, TypeInType,
    GADTs, RecordWildCards, StandaloneDeriving #-}
module Union where

import Data.Kind ( Type )

type family InUnion (a :: Type) (b :: Type) (c :: Type) where {}



data Union a b where
  MkUn :: InUnion a b c => c -> Union a b


-- should type check

x :: Union Bool Int
x = MkUn True


y :: Union Bool Int
y = MkUn (3 :: Int)

