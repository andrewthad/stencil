{-# language GADTs #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language PolyKinds #-}

module Data.Bin.Omni
  ( Omni(..)
  , absurd
  ) where

import Data.Nat (Nat(..))
import Data.Bin.Fin (Fin(..))
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

data Gte :: Nat -> Nat -> Type where
  GteEq :: Gte n n
  GteGt :: Gte m ('Succ n) -> Gte m n

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

