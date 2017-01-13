{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}  -- for: class Insertable s n where
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

-- {-# LANGUAGE FlexibleInstances #-}      -- for: instance VTInsertable 1 0 where

-- Without this, "Could not deduce: ((n1 + n) + 1) ~ (m + n)"
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}


module VecTime where

import GHC.TypeLits
import Data.Proxy

import InTime
import Vec
import Constraints

-----

-- Need to do this with a typeclass?

{-
vtAppend = (+++)
infixr 5 +++

(+++) ∷ (KnownNat m, KnownNat m, KnownNat (m+n)) => Vec m α → Vec n α → InTime (1 + 2*m) (Vec (m+n) α)
(+++) Nil       ys = tReturn1 ys
(+++) (x :# xs) ys = tStep1 $ (xs +++ ys) `tBind` \xsys → tReturn1 $ x :# xsys
-}

-----

-- (◂)
vtInsert ∷ (KnownNat n, Ord α) ⇒ α → Vec n α → InTime (n+1) (Vec (n+1) α)
vtInsert = (+$+)
infixr 5 +$+

-- ^ We use tReturnN to declare the number of steps it MIGHT be.
-- http://stackoverflow.com/questions/40731220/couldnt-match-kind-with-nat
-- (+$+) ∷ ∀ n α. (KnownNat n, KnownNat (n-1), Ord α)
(+$+) ∷ ∀ n α. (KnownNat n, Ord α)
      ⇒ α → Vec n α → InTime (n+1) (Vec (n+1) α)

(+$+) x Nil =
  tReturn1 (x :# Nil)

(+$+) x ys@(y :# ys')
  | x < y =
      tReturnN (Proxy ∷ Proxy (n+1)) (x :# ys)
  | otherwise =
      tStep1 $ (x +$+ ys') `tBind` \xys' → tReturn $ y :# xys'


{-
(n ^ 2 + n) + (n + 1) ≡ (1 + n) ^ 2
(n+1) * (n+1)  =  n² + n + n + 1  =  n² + 2*n + 1
-}

-- vtInSort ∷ (Ord α) ⇒ Vec n α → InTime (n^2 - n + 1) (Vec n α)
-- vtInSort ∷ (Ord α) ⇒ Vec n α → InTime (n^2 + n) (Vec n α)
-- vtInSort Nil = tReturn Nil
-- vtInSort (x :# xs) =
-- vtInSort xs `tBind` vtInsert x `tBind` tReturn

-- type family 

xyz ∷ Int → Int
xyz 0 = 0
xyz i = (prevI * (xyz prevI)) + i
  where prevI = i-1

-- i r
-- ----
-- 0 0
-- 1 1
-- 2 3
-- 3 6
-- 4 10
-- 5 15
-- 6 21

-- 0 0
-- 1 0
-- 2 1
-- 3 3
-- 4 6
-- 5 10
-- 6 15

-- n ≡ 6, steps: 15 + 6 + 0 = 21
-- n ≡ 5, steps: 10 + 5 + 0 = 15
-- n ≡ 4, steps: 6  + 4 + 0 = 10
-- n ≡ 3, steps: 3  + 3 + 0 = 6
-- n ≡ 2, steps: 1  + 2 + 0 = 3
-- n ≡ 1, steps: 0  + 1 + 0 = 1
-- n ≡ 0, steps: 0 = 0
