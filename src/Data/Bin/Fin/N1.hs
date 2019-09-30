{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Data.Bin.Fin.N1
  ( N0
  , N1
  , n0
  , n1
  , pattern N0
  , pattern N1
  ) where

import Data.Bin.Fin (Fin(..),SingFin(..))
import qualified Data.Bin.Fin.N0 as N0
import qualified Data.Nat as Nat

type N0 = 'Une N0.N0
type N1 = 'Duo N0.N0

pattern N0 :: () => (n ~ 'Une 'Nil) => SingFin @Nat.N1 n
pattern N0 = SingUne SingNil

pattern N1 :: () => (n ~ 'Duo 'Nil) => SingFin @Nat.N1 n
pattern N1 = SingDuo SingNil

n0 :: SingFin N0
n0 = SingUne SingNil

n1 :: SingFin N1
n1 = SingDuo SingNil
