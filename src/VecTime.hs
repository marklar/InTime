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
(+++) Nil       ys = tStepRet ys
(+++) (x :# xs) ys = tStep $ (xs +++ ys) `tBind` \xsys → tStepRet $ x :# xsys

-----

-- (◂)
vtInsert ∷ (Ord α) ⇒ α → Vec n α → InTime (n+1) (Vec (n+1) α)
vtInsert = (+$+)
infixr 5 +$+

{-| We use stepN to declare the number of steps it MIGHT be.
-}
(+$+) ∷ (Ord α) ⇒ α → Vec n α → InTime (n+1) (Vec (n+1) α)
(+$+) x Nil =
  stepN (undefined ∷ Proxy 1) $ tReturn (x :# Nil)
(+$+) x ys@(y :# ys')
  | x < y =
      stepN (undefined ∷ Proxy (n + 1)) $ tReturn (x :# ys)
  | otherwise =
      {- tStep $ -}
      (x +$+ ys') `tBind` \xys' → tStepRet $ y :# xys'
