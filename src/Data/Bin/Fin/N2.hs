{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Data.Bin.Fin.N2
  ( N0
  , N1
  , N2
  , N3
  , pattern N0
  , pattern N1
  , pattern N2
  , pattern N3
  ) where

import Data.Bin.Fin (Fin(..),SingFin(..))
import qualified Data.Bin.Fin.N1 as N1
import qualified Data.Nat as Nat

type N0 = 'Une N1.N0
type N1 = 'Duo N1.N0
type N2 = 'Une N1.N1
type N3 = 'Duo N1.N1

pattern N0 :: () => (n ~ 'Une N1.N0) => SingFin @Nat.N2 n
pattern N0 = SingUne N1.N0

pattern N1 :: () => (n ~ 'Duo N1.N0) => SingFin @Nat.N2 n
pattern N1 = SingDuo N1.N0

pattern N2 :: () => (n ~ 'Une N1.N1) => SingFin @Nat.N2 n
pattern N2 = SingUne N1.N1

pattern N3 :: () => (n ~ 'Duo N1.N1) => SingFin @Nat.N2 n
pattern N3 = SingDuo N1.N1
