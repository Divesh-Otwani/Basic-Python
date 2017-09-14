{-# LANGUAGE GADTs #-}
module LambdaCalc where


newtype Id = Id String

data Term where
  I :: Id -> Term
  L :: Id -> Term -> Term
  A :: Term -> Term -> Term


















































