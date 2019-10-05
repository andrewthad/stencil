{-# language GADTs #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}

module Data.Bin.Omni
  ( Omni(..)
  , absurd
  , fromFunction
  ) where

import Data.Nat (Nat(..),SingNat(..))
import Data.Bin.Fin (Fin(..),SingFin(..))
import Data.Kind (Type)

data Omni :: forall
    (h :: Nat)
    (n :: Nat).
    (Fin h -> Type) ->
    Fin n ->
    Type
  where
  Leaf :: i v -> Omni @h @h i v
  Branch ::
       Omni @h @('Succ n) i ('Une v)
    -> Omni @h @('Succ n) i ('Duo v)
    -> Omni @h @n i v

fromFunction :: forall h i.
     SingNat h
  -> (forall v. SingFin @h v -> i v)
  -> Omni @h @'Zero i 'Nil
fromFunction h0 f = go (gteZero h0) SingNil where
  go :: forall m w. Gte h m -> SingFin @m w -> Omni @h @m i w
  go GteEq v = Leaf (f v)
  go (GteGt gt) v = Branch (go gt (SingUne v)) (go gt (SingDuo v))

data Gte :: Nat -> Nat -> Type where
  GteEq :: Gte n n
  GteGt :: Gte m ('Succ n) -> Gte m n

gteZero :: forall n. SingNat n -> Gte n 'Zero
gteZero = go GteEq where
  go :: forall m. Gte n m -> SingNat m -> Gte n 'Zero
  go gt SingZero = gt
  go gt (SingSucc x) = go (GteGt gt) x

absurd :: Omni @n @('Succ n) i v -> a
{-# noinline absurd #-}
absurd r = impossibleGte (omniToGte r)

impossibleGte :: forall (m :: Nat) (a :: Type). Gte m ('Succ m) -> a
impossibleGte (GteGt g) = impossibleGte (predGte g)

predGte :: Gte m ('Succ n) -> Gte m n
predGte GteEq = GteGt GteEq
predGte (GteGt g) = GteGt (predGte g)

omniToGte :: Omni @h @n i v -> Gte h n
omniToGte (Branch t _) = GteGt (omniToGte t)
omniToGte (Leaf _) = GteEq

