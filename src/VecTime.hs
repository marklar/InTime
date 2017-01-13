{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}      -- for: instance VTInsertable 1 0 where
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}  -- for: class Insertable s n where
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- ??
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}


-- Without this, "Could not deduce: ((n1 + n) + 1) ~ (m + n)"
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}


module VecTime where

import GHC.TypeLits
import Data.Kind hiding (type (*))
import Data.Proxy

import InTime
import Vec
import Constraints

-----

-- Need to do this with a typeclass?

vtAppend = (+++)
infixr 5 +++

(+++) ∷ Vec m α → Vec n α → InTime (1 + 2*m) (Vec (m+n) α)
(+++) Nil       ys = tReturn1 ys
(+++) (x :# xs) ys = tStep1 $ (xs +++ ys) `tBind` \xsys → tReturn1 $ x :# xsys

-----

-- (◂)
vtInsert ∷ (Ord α) ⇒ α → Vec n α → InTime (n+1) (Vec (n+1) α)
vtInsert = (+$+)
infixr 5 +$+

-- ^ We use tReturnN to declare the number of steps it MIGHT be.
-- http://stackoverflow.com/questions/40731220/couldnt-match-kind-with-nat
(+$+) ∷ ∀ n α. (Ord α)
      ⇒ α → Vec n α → InTime (n+1) (Vec (n+1) α)

(+$+) x Nil =
  tReturn1 (x :# Nil)

(+$+) x ys@(y :# ys')
  | x < y =
      tReturnN (Proxy ∷ Proxy (n+1)) (x :# ys)
  | otherwise =
      tStep1 $ (x +$+ ys') `tBind` \xys' → tReturn $ y :# xys'


{-| Similar to Fibonacci series.
n : s  i  total
---------------
0 : 0  -  0
1 : 0  1  1
2 : 1  2  3
3 : 3  3  6
4 : 6  4  10
5 : 10 5  15
6 : 15 6  21
-}

type family InSortSteps (n ∷ Nat) where
  InSortSteps 0 = 0
  InSortSteps n = n + InSortSteps (n-1)

{-
vtInSort ∷ (Ord α) ⇒ Vec n α → InTime (InSortSteps n) (Vec n α)
vtInSort Nil = tReturn Nil
vtInSort (x :# xs) =
  vtInSort xs `tBind` vtInsert x `tBind` tReturn
-}

{-
class InSortable n where
  vtInSort ∷ Vec n α → InTime (InSortSteps n) (Vec n α)

instance InSortable 0 where
  vtInSort Nil = tReturn Nil

instance (n > 0, InSortable (n-1)) ⇒ InSortable n where
  vtInSort (x :# xs) = vtInSort xs `tBind` vtInsert x `tBind` tReturn
-}

{-
class InSortable v where
  vtInSort ∷ (KnownNat n) ⇒ v α → InTime (InSortSteps n) (v α)

instance InSortable (Vec 0) where
  vtInSort Nil = tReturn Nil

instance (n > 0, InSortable (Vec (n-1))) ⇒ InSortable (Vec n) where
  vtInSort (x :# xs) = vtInSort xs `tBind` vtInsert x `tBind` tReturn
-}
