{-# LANGUAGE DataKinds #-}              -- for: TypeLits (e.g. 0, 1) in type sig
{-# LANGUAGE FlexibleInstances #-}      -- for: instance Wait n InTime
{-# LANGUAGE KindSignatures #-}         -- for: kinds (e.g. Type, Nat) in sig of GADT
{-# LANGUAGE MultiParamTypeClasses #-}  -- for: class Wait n c where
{-# LANGUAGE TypeOperators #-}          -- for: (>), (+), etc. in type sig
{-# LANGUAGE UnicodeSyntax #-}

-- Without this, "Couldn't match type ‘2 + n’ with ‘n + 2’"
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-|
Data ctor for InTime isn't exported.
(Clients must use `tReturn` instead.)
-}
module InTime where
  -- ( InTime
  -- , tBind
  -- , tReturn
  -- , tForce
  -- , tStep
  -- , tStepRet
  -- ) where


import Data.Proxy
import GHC.TypeLits
-- import Constraints


data InTime (n ∷ Nat) α = IT α
  deriving (Show, Eq)


-------------------------
-- Monad-esque methods

tBind ∷ InTime m α → (α → InTime n β) → InTime (m + n) β
tBind (IT x) f = g (f x)
  where g (IT y) = IT y


tReturn ∷ α → InTime 0 α
tReturn = IT


tReturn1 ∷ α → InTime 1 α
tReturn1 x = IT x


tReturnN ∷ Proxy n → α → InTime n α
tReturnN _ = IT


-- look for dual of Monad
tForce ∷ InTime n α → α
tForce (IT x) = x


-------------------
-- steps

tStep1 ∷ InTime n α → InTime (n + 1) α
tStep1 (IT x) = IT x


{-| Example usage:
  $ let v0 = 1 :# 2 :# Nil ∷ Vec 2 Int
  $ :t stepN (Proxy ∷ Proxy 10) (tReturn v0)
  > … ∷ (IT v0 ∷ InTime 10 (Vec 2 Int))
-}
tStepN ∷ Proxy m → InTime n α → InTime (m+n) α
tStepN _ (IT x) = IT x
