{-# LANGUAGE
    GADTs, TypeInType, ScopedTypeVariables,
    TypeFamilies, TypeOperators,
    TypeApplications, UndecidableInstances,
    ConstraintKinds
#-}

module NativeLTE where

import Data.Kind

{-
In this module I will attempt to
represent constraints on native data types.

This failed misrably.

-}

{- Failure.
type family (n :: Int) + (m :: Int) :: Int where


data SInt :: Int -> * where
  SZero :: SInt 0
  SSucc :: SInt n -> SInt (n+1)
  SPred :: SInt n -> SInt (n-1)


-}





