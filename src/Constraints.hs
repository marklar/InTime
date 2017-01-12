{-# LANGUAGE ConstraintKinds #-}     -- CmpNat x y ~ 'GT
{-# LANGUAGE DataKinds #-}           -- to use TypeLits in type sig
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}       -- to use (>), (+), etc. in type sig
{-# LANGUAGE UnicodeSyntax #-}

module Constraints where

import Data.Kind
import GHC.TypeLits

-- This are Constraints. (e.g. `n < 1` or `1 > n`)
type x > y = CmpNat x y ~ 'GT
type x < y = CmpNat x y ~ 'LT

-- type Foo x y = (x > y, x < y)
