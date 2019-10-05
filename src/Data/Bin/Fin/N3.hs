{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Data.Bin.Fin.N3
  ( N0
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
  , N7
  , pattern N0
  , pattern N1
  , pattern N2
  , pattern N3
  , pattern N4
  , pattern N5
  , pattern N6
  , pattern N7
  ) where

import Data.Bin.Fin (Fin(..),SingFin(..))
import qualified Data.Bin.Fin.N2 as N2
import qualified Data.Nat as Nat

type N0 = 'Une N2.N0
type N1 = 'Duo N2.N0
type N2 = 'Une N2.N1
type N3 = 'Duo N2.N1
type N4 = 'Une N2.N2
type N5 = 'Duo N2.N2
type N6 = 'Une N2.N3
type N7 = 'Duo N2.N3

pattern N0 :: () => (n ~ 'Une N2.N0) => SingFin @Nat.N3 n
pattern N0 = SingUne N2.N0

pattern N1 :: () => (n ~ 'Duo N2.N0) => SingFin @Nat.N3 n
pattern N1 = SingDuo N2.N0

pattern N2 :: () => (n ~ 'Une N2.N1) => SingFin @Nat.N3 n
pattern N2 = SingUne N2.N1

pattern N3 :: () => (n ~ 'Duo N2.N1) => SingFin @Nat.N3 n
pattern N3 = SingDuo N2.N1

pattern N4 :: () => (n ~ 'Une N2.N2) => SingFin @Nat.N3 n
pattern N4 = SingUne N2.N2

pattern N5 :: () => (n ~ 'Duo N2.N2) => SingFin @Nat.N3 n
pattern N5 = SingDuo N2.N2

pattern N6 :: () => (n ~ 'Une N2.N3) => SingFin @Nat.N3 n
pattern N6 = SingUne N2.N3

pattern N7 :: () => (n ~ 'Duo N2.N3) => SingFin @Nat.N3 n
pattern N7 = SingDuo N2.N3
