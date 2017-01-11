{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Without this, "Could not deduce: ((n1 + n) + 1) ~ (m + n)"
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module VecTime where

import GHC.TypeLits

import InTime
import Vec


vtAppend ∷ Vec m α → Vec n α → InTime (1 + 2*m) (Vec (m+n) α)
vtAppend Nil       ys = tStepRet ys
vtAppend (x :# xs) ys =
  tStep $ (xs `vtAppend` ys) `tBind` \ xsys → tStepRet $ x :# xsys

(+++) = vtAppend
infixr 5 +++
