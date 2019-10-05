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
  , singleton
  , map
  , traverse
  , traverse_
  , union
  , zip_
  ) where

import Prelude hiding (Either(Left,Right),map,traverse)

import Control.Applicative (liftA2)
import Data.Nat (Nat(..))
import Data.Bin.Fin (Fin(..),SingFin(..))
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

traverse ::
     forall (h :: Nat) (n :: Nat) (k :: Type)
     (w :: Fin n) (s :: Map k h n)
     (i :: k -> Fin h -> Type) (e :: Fin h -> Type)
     (j :: k -> Fin h -> Type) (m :: Type -> Type).
     Applicative m
  => (forall (v :: Fin h) (y :: k). e v -> i y v -> m (j y v))
  -> Omni @h @n e w -- environment
  -> Rec @h @n @k i s w -- argument tree
  -> m (Rec @h @n @k j s w)
traverse _ !_ Empty = pure Empty
traverse _ !(Omni.Branch t _) (Leaf _) = Omni.absurd t
traverse f !(Omni.Leaf v) (Leaf x) = fmap Leaf (f v x)
traverse _ !(Omni.Leaf _) (Left t) = absurd t
traverse _ !(Omni.Leaf _) (Right t) = absurd t
traverse _ !(Omni.Leaf _) (Branch t _) = absurd t
traverse f !(Omni.Branch x _) (Left a) = fmap Left (traverse f x a)
traverse f !(Omni.Branch _ y) (Right b) = fmap Right (traverse f y b)
traverse f !(Omni.Branch x y) (Branch a b) =
  liftA2 Branch (traverse f x a) (traverse f y b)

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

-- It is not possible to generalize this function further without rewriting
-- the Singleton type family. The Zero in the resulting Tree is dictated by
-- the result kind of the Singleton type family. 
singleton ::
     forall (h :: Nat) (n :: Nat) (k :: Type)
            (w :: Fin n) (s :: Map k h n) (i :: k -> Fin h -> Type).
     SingFin @n w -- Finger to node under consideration
  -> Rec @h @n i s w
  -> Rec @h @'Zero i (Map.Singleton w s) 'Nil
singleton SingNil s = s
singleton (SingUne bs) s = singleton bs (Left s)
singleton (SingDuo bs) s = singleton bs (Right s)

-- Union that lets the user choose what to do when only one of the
-- two is present.
union ::
  forall (h :: Nat) (n :: Nat) (k :: Type)
  (w :: Fin n) (r :: Map k h n) (s :: Map k h n)
  (i :: k -> Fin h -> Type).
     Rec @h @n @k i r w
  -> Rec @h @n @k i s w
  -> Rec @h @n @k i (Map.Union r s) w
union Empty t = t
union (Leaf x) Empty = Leaf x
union (Leaf x) (Leaf _) = Leaf x
union (Left x) (Left y) = Left (union x y)
union (Branch xl xr) (Right yr) = Branch xl (union xr yr)
union (Branch xl xr) (Left yl) = Branch (union xl yl) xr
union (Branch xl xr) (Branch yl yr) = Branch (union xl yl) (union xr yr)
union (Left xl) (Branch yl yr) = Branch (union xl yl) yr
union (Right xr) (Branch yl yr) = Branch yl (union xr yr)
union (Right x) (Right y) = Right (union x y)
union (Leaf _) (Branch t _) = absurd t
union (Branch t _) (Leaf _) = absurd t
union (Branch xl xr) Empty = Branch xl xr
union (Left x) Empty = Left x
union (Right x) Empty = Right x
union (Left x) (Right y) = Branch x y
union (Right x) (Left y) = Branch y x
union (Left t) (Leaf _) = absurd t
union (Right t) (Leaf _) = absurd t
union (Leaf _) (Left t) = absurd t
union (Leaf _) (Right t) = absurd t

-- | Zip the trees together. Record fields must match exactly.
zip_ :: 
     forall (h :: Nat) (n :: Nat) (k :: Type) (w :: Fin n) (r :: Map k h n)
     (i :: k -> Fin h -> Type) (j :: k -> Fin h -> Type)
     (e :: Fin h -> Type) (m :: Type).
     Monoid m
  => (forall (v :: Fin h) (y :: k). e v -> i y v -> j y v -> m)
  -> Omni @h @n e w -- environment
  -> Rec @h @n i r w -- left tree
  -> Rec @h @n j r w -- right tree
  -> m
zip_ _ !_ Empty Empty = mempty
zip_ f (Omni.Leaf e) (Leaf x) (Leaf y) = f e x y
zip_ f (Omni.Branch e _) (Left a) (Left b) = zip_ f e a b
zip_ f (Omni.Branch _ e) (Right a) (Right b) = zip_ f e a b
zip_ f (Omni.Branch el er) (Branch a1 a2) (Branch b1 b2) =
  zip_ f el a1 b1 <> zip_ f er a2 b2
zip_ _ (Omni.Branch t _) (Leaf _) _ = Omni.absurd t
zip_ _ (Omni.Leaf _) (Left t) _ = absurd t
zip_ _ (Omni.Leaf _) (Right t) _ = absurd t
zip_ _ (Omni.Leaf _) (Branch t _) _ = absurd t

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

