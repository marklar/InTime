{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}  -- for: class Insertable s n where
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}


-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}       -- for: instance VTInsertable 1 0 where
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE IncoherentInstances #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE ScopedTypeVariables #-}



-- Without this, "Could not deduce: ((n1 + n) + 1) ~ (m + n)"
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}


module VecTime where

import GHC.TypeLits
import Data.Proxy

import InTime
import Vec
import Constraints

{-
data (≤) ∷ Nat → Nat → * where
  EQT ∷ x ≤ x
  -- EQT ∷ x ≤ (x + y)
  LTT ∷ x ≤ y → x ≤ (y+1)    -- (x + y)

foo ∷ 3 ≤ 5
foo = LTT (LTT EQT)
-}


vtAppend = (+++)

infixr 5 +++
(+++) ∷ Vec m α → Vec n α → InTime (1 + 2*m) (Vec (m+n) α)
(+++) Nil       ys = tStepRet ys
(+++) (x :# xs) ys = tStep $ (xs +++ ys) `tBind` \xsys → tStepRet $ x :# xsys


{-
-- NB: For TypeLits.(<=), cannot use (≤), must use (<=).
vtInsert ∷ (Ord α, s <= (n+1))
         ⇒ α → Vec n α → InTime s (Vec (n+1) α)
vtInsert x Nil = tStepRet $ x :# Nil
vtInsert x (y :# ys)
  | x < y     = tStepRet (x :# y :# ys)
  | otherwise = tStep $ (vtInsert x ys) `tBind` \xys → tStepRet $ y :# xys
-}

-- s: time steps
-- n: Vec length
class VTInsertable (s ∷ Nat) (n ∷ Nat) where
  vtInsert ∷ α → Vec n α → InTime s (Vec (n+1) α)

instance VTInsertable 1 0 where
  vtInsert x Nil = tStepRet (x :# Nil)

{-
At first tStepRet, can't deduce: s ~ 1
    Expected type: InTime s (Vec (n+1) α)
      Actual type: InTime 1 (Vec (n+1) α)
-}
instance (n > 0, VTInsertable s (n-1)) ⇒ VTInsertable s n where
  vtInsert x (y :# ys) = undefined
--     | x < y     = tStepRet (x :# y :# ys)
--     | otherwise = tStep $ (vtInsert x ys) `tBind` \xys → tStepRet $ y :# xys
