{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Data.Bin.Rec
  ( Rec(..)
  , map
  , traverse_
  ) where

import Prelude hiding (Either(Left,Right),map)

import Data.Nat (Nat(..))
import Data.Bin.Fin (Fin(..))
import Data.Kind (Type)
import Data.Bin.Map (Map)
import Data.Bin.Omni (Omni)

import qualified Data.Bin.Map as Map
import qualified Data.Bin.Omni as Omni

data Rec ::
    forall (h :: Nat). -- Total height
    forall (n :: Nat).
    forall (k :: Type).
    (k -> Fin h -> Type) -> -- Interpreter
    Map k h n -> -- Set of fields
    Fin n -> -- Finger to position in set
    Type
  where
  Leaf :: forall
       (h :: Nat)
       (k :: Type)
       (i :: k -> Fin h -> Type)
       (v :: Fin h)
       (a :: k)
     . i a v
    -> Rec @h @h i ('Map.Leaf a) v
  Left ::
       Rec @h @('Succ n) i sl ('Une v)
    -> Rec @h @n i ('Map.Left sl) v
  Right ::
       Rec @h @('Succ n) @k i s ('Duo v)
    -> Rec @h @n @k i ('Map.Right s) v
  Branch ::
       Rec @h @('Succ n) @k i sl ('Une v)
    -> Rec @h @('Succ n) @k i sr ('Duo v)
    -> Rec @h @n @k i ('Map.Branch sl sr) v
  Empty :: Rec @h @'Zero @k i 'Map.Empty 'Nil

traverse_ ::
     forall (h :: Nat) (n :: Nat) (k :: Type) (w :: Fin n) (s :: Map k h n)
     (i :: k -> Fin h -> Type) (j :: Fin h -> Type) (m :: Type -> Type).
     Applicative m
  => (forall (v :: Fin h) (y :: k). j v -> i y v -> m ())
  -> Omni @h @n j w -- environment
  -> Rec @h @n @k i s w -- argument tree
  -> m ()
traverse_ _ !_ Empty = pure ()
traverse_ _ !(Omni.Branch t _) (Leaf _) = Omni.absurd t
traverse_ f !(Omni.Leaf v) (Leaf x) = f v x
traverse_ _ !(Omni.Leaf _) (Left t) = absurd t
traverse_ _ !(Omni.Leaf _) (Right t) = absurd t
traverse_ _ !(Omni.Leaf _) (Branch t _) = absurd t
traverse_ f !(Omni.Branch x _) (Left a) = traverse_ f x a
traverse_ f !(Omni.Branch _ y) (Right b) = traverse_ f y b
traverse_ f !(Omni.Branch x y) (Branch a b) =
     traverse_ f x a
  *> traverse_ f y b

map ::
     forall (h :: Nat) (n :: Nat) (k :: Type) (w :: Fin n) (s :: Map k h n)
     (i :: k -> Fin h -> Type) (e :: Fin h -> Type) (j :: k -> Fin h -> Type).
     (forall (v :: Fin h) (y :: k). e v -> i y v -> j y v)
  -> Omni @h @n e w
  -> Rec @h @n i s w -- argument tree
  -> Rec @h @n j s w
map _ !_ Empty = Empty
map _ (Omni.Branch t _) (Leaf _) = Omni.absurd t
map _ (Omni.Leaf _) (Branch t _) = absurd t
map _ (Omni.Leaf _) (Left t) = absurd t
map _ (Omni.Leaf _) (Right t) = absurd t
map f (Omni.Branch _ y) (Right b) = Right (map f y b)
map f (Omni.Branch x _) (Left a) = Left (map f x a)
map f (Omni.Branch x y) (Branch a b) = Branch (map f x a) (map f y b)
map f (Omni.Leaf v) (Leaf x) = Leaf (f v x)

data Gte :: Nat -> Nat -> Type where
  GteEq :: Gte n n
  GteGt :: Gte m ('Succ n) -> Gte m n

absurd :: forall (k :: Type) (n :: Nat) (v :: Fin ('Succ n))
    (i :: k -> Fin n -> Type) (s :: Map k n ('Succ n)) (a :: Type).
  Rec @n @('Succ n) i s v -> a
{-# noinline absurd #-}
absurd r = impossibleGte (recToGte r)

impossibleGte :: forall (m :: Nat) (a :: Type). Gte m ('Succ m) -> a
impossibleGte (GteGt g) = impossibleGte (predGte g)

predGte :: Gte m ('Succ n) -> Gte m n
predGte GteEq = GteGt GteEq
predGte (GteGt g) = GteGt (predGte g)

recToGte :: Rec @h @('Succ n) i s v -> Gte h ('Succ n)
recToGte (Leaf _) = GteEq
recToGte (Branch t _) = GteGt (recToGte t)
recToGte (Left t) = GteGt (recToGte t)
recToGte (Right t) = GteGt (recToGte t)
