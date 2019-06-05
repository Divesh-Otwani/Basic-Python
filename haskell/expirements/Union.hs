{-# LANGUAGE TypeFamilies, TypeInType,
    GADTs, RecordWildCards, StandaloneDeriving #-}
module Union ( Union(..) ) where

import Data.Kind ( Type )

{-
All we expose is the data type, not the type family.
Hence, you can make arbtrary unions and it avoids the
tau space problem.

-}

type family InUnion (a :: Type) (b :: Type) (c :: Type) where {}



data Union a b where
  MkUn :: InUnion a b c => c -> Union a b


-- should type check

x :: Union Bool Int
x = MkUn True


y :: Union Bool Int
y = MkUn (3 :: Int)

