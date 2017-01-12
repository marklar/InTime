{-# LANGUAGE ConstraintKinds #-}     -- CmpNat x y ~ 'GT
{-# LANGUAGE DataKinds #-}           -- to use TypeLits in type sig
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}       -- to use (>), (+), etc. in type sig
{-# LANGUAGE UnicodeSyntax #-}

module Constraints where

import Data.Kind hiding (type (*))
import GHC.TypeLits
-- import Data.Maybe


-- This are Constraints. (e.g. `n < 1` or `1 > n`)
type x > y = CmpNat x y ~ 'GT
type x < y = CmpNat x y ~ 'LT

-----------------


-- type Foo x y = (x > y, x < y)


{-| Though 'x' is an Integer, we get it from length of Vector,
so we know it's non-negative (and thus isomorphic to a Nat).
--
natIntegerAsSomeNat ∷ Integer → SomeNat
natIntegerAsSomeNat x =
  fromMaybe (SomeNat (Proxy ∷ Proxy 0)) (someNatVal x)
-}


-- (≤) is a type
-- data ctor EQT crts data of type (x ≤ x)
-- data ctor LTT crts data of type (x ≤ y) → x ≤ (y+1)
data (≤) ∷ Nat → Nat → Type where
  EQT ∷ x ≤ x
  -- EQT ∷ x ≤ (x + y)
  LTT ∷ x ≤ y → x ≤ (y+1)    -- (x + y)

foo ∷ 3 ≤ 5
foo = LTT (LTT EQT)

-- bar ∷ 3 ≤ 4
bar ∷ 8 ≤ 9
bar = LTT EQT

baz ∷ 3 ≤ 3
baz = EQT

-- quux ∷ (n ≡ m) ⇒ n ≤ m
-- quux = EQT

data Deficit ∷ Nat → Nat → Type where
  Equal ∷ Deficit x x
  OneLess ∷ Deficit x y → Deficit x (y+1)

z ∷ Deficit 3 6
z = OneLess . OneLess . OneLess $ Equal
