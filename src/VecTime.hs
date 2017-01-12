{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}  -- for: class Insertable s n where
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE FlexibleInstances #-}      -- for: instance VTInsertable 1 0 where
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- Without this, "Could not deduce: ((n1 + n) + 1) ~ (m + n)"
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}


module VecTime where

import GHC.TypeLits
import Data.Proxy

import InTime
import Vec
import Constraints

-----

vtAppend = (+++)
infixr 5 +++

(+++) ∷ Vec m α → Vec n α → InTime (1 + 2*m) (Vec (m+n) α)
(+++) Nil       ys = tReturn1 ys
(+++) (x :# xs) ys = tStep1 $ (xs +++ ys) `tBind` \xsys → tReturn1 $ x :# xsys

-----

-- {-
-- (◂)
vtInsert ∷ (Ord α) ⇒ α → Vec n α → InTime (n+1) (Vec (n+1) α)
vtInsert = (+$+)
infixr 5 +$+

-- ^ We use tStepN to declare the number of steps it MIGHT be.
(+$+) ∷ (Ord α) ⇒ α → Vec n α → InTime (n+1) (Vec (n+1) α)
(+$+) x Nil =
  tReturn1 (x :# Nil)
(+$+) x ys@(y :# ys')
  | x < y =
      tStepN (undefined ∷ Proxy (n + 1)) $ tReturn (x :# ys)
  | otherwise =
      {- tStep1 $ -}
      (x +$+ ys') `tBind` \xys' → tReturn1 $ y :# xys'
-- |-}


{-
(+$+) ∷ (Ord α) ⇒ α → Vec n α → InTime n (Vec (n+1) α)
(+$+) x Nil =
  tReturn (x :# Nil)
(+$+) x ys@(y :# ys')
  | x < y =
      tStepN (Proxy ∷ Proxy n) $ tReturn (x :# ys)
  | otherwise =
      {- tStep1 $ -}
      (x +$+ ys') `tBind` \xys' → tReturn1 $ y :# xys'
-}

{-
(n ^ 2 + n) + (n + 1) ≡ (1 + n) ^ 2
(n+1) * (n+1)  =  n² + n + n + 1  =  n² + 2*n + 1
-}

{-
-- vtInSort ∷ (Ord α) ⇒ Vec n α → InTime ((n+1)^2) (Vec n α)
-- vtInSort ∷ (Ord α) ⇒ Vec n α → InTime (n^2) (Vec n α)
vtInSort ∷ (Ord α) ⇒ Vec n α → InTime (n^2 - n + 1) (Vec n α)
vtInSort Nil = tReturn1 Nil
vtInSort (x :# xs) =
  vtInSort xs `tBind` vtInsert x `tBind` tReturn
-}
