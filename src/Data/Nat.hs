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
    -- Constant Values
  , n0
  , n1
  , n2
  , n3
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

n0 :: SingNat N0
n0 = SingZero

n1 :: SingNat N1
n1 = SingSucc n0

n2 :: SingNat N2
n2 = SingSucc n1

n3 :: SingNat N3
n3 = SingSucc n2
