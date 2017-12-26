{-# LANGUAGE GADTs #-}
module EitherMonad where


data Error where
  Error :: Error


type MaybeInt = Either Error Int

mhalf :: Int -> MaybeInt
mhalf 0 = Left Error
mhalf i = Right i


mmult4 :: Int -> MaybeInt
mmult4 x =
  do
    x' <- mhalf x
    return $ x' * 4










