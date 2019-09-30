{-# language GADTs #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}

module Data.Bin.Map
  ( Map(..)
  , Union
  ) where

import Prelude hiding (Either(Left,Right))

import Data.Nat (Nat(..))
import Data.Kind (Type)

data Map :: Type -> Nat -> Nat -> Type where
  Leaf :: forall (k :: Type) (h :: Nat).
       k
    -> Map k h h
  Branch ::
       Map k h ('Succ n)
    -> Map k h ('Succ n)
    -> Map k h n
  Left :: Map k h ('Succ n) -> Map k h n
  Right :: Map k h ('Succ n) -> Map k h n
  Empty :: Map k h 'Zero

-- This is left-biased once we get down to the leaves.
-- Csongor's unsaturated type families are badly needed here.
type family Union (x :: Map k h n) (y :: Map k h n) :: Map k h n where
  Union ('Leaf x) ('Leaf _) = 'Leaf x
  Union ('Leaf x) 'Empty = 'Leaf x
  Union 'Empty s = s
  Union ('Left x) 'Empty = 'Left x
  Union ('Branch xl xr) ('Left yl) = 'Branch (Union xl yl) xr
  Union ('Branch xl xr) ('Right yr) = 'Branch xl (Union xr yr)
  Union ('Left xl) ('Branch yl yr) = 'Branch (Union xl yl) yr
  Union ('Right xr) ('Branch yl yr) = 'Branch yl (Union xr yr)
  Union ('Left x) ('Left y) = 'Left (Union x y)
  Union ('Left x) ('Right y) = 'Branch x y
  Union ('Right x) ('Right y) = 'Right (Union x y)
  Union ('Right x) ('Left y) = 'Branch y x
  Union ('Right x) 'Empty = 'Right x
  Union ('Branch xl xr) 'Empty = 'Branch xl xr
  Union ('Branch xl xr) ('Branch yl yr) = 'Branch (Union xl yl) (Union xr yr)
