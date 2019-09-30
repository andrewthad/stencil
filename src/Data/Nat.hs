{-# language GADTs #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}

module Data.Nat
  ( Nat(..)
  , SingNat(..)
    -- Constant Types
  , N0
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
  , N7
  ) where

import Data.Kind (Type)

data Nat = Zero | Succ Nat

data SingNat :: Nat -> Type where
  SingZero :: SingNat 'Zero
  SingSucc :: SingNat n -> SingNat ('Succ n)

type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2
type N4 = 'Succ N3
type N5 = 'Succ N4
type N6 = 'Succ N5
type N7 = 'Succ N6

