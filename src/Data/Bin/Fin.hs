{-# language GADTs #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language PolyKinds #-}

module Data.Bin.Fin
  ( Fin(..)
  , SingFin(..)
  ) where

import Data.Nat (Nat(Zero,Succ))
import Data.Kind (Type)

-- | A binary finite natural number whose bound is given by @2^B@.
data Fin :: Nat -> Type where
  Nil :: Fin 'Zero
  -- ^ The number 0 is less than @2^0@ (@2^0 = 1@)
  Une :: Fin n -> Fin ('Succ n) 
  -- ^ Given @N < 2^B@, produces a proof that @2*N < 2^(B+1)@
  Duo :: Fin n -> Fin ('Succ n)
  -- ^ Given @N < 2^B@, produces a proof that @2*N+1 < 2^(B+1)@

data SingFin :: forall (n :: Nat). Fin n -> Type where
  SingNil :: SingFin 'Nil
  SingUne :: SingFin f -> SingFin ('Une f)
  SingDuo :: SingFin f -> SingFin ('Duo f)
