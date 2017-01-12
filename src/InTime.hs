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


-- look for dual of Monad
tForce ∷ InTime n α → α
tForce (IT x) = x


-------------------
-- steps

tStep ∷ InTime n α → InTime (n + 1) α
tStep (IT x) = IT x


-- ~ "tStep . tReturn"
tStepRet ∷ α → InTime 1 α
tStepRet x = IT x


-------------------
-- multiple steps

step2 ∷ InTime n α → InTime (n+2) α
step2 (IT x) = IT x


step3 ∷ InTime n α → InTime (n+3) α
step3 (IT x) = IT x


---------------------------
-- arbitrarily many steps

{-
-- This doesn't work: "Expected a type, but 'm' has kind 'Nat'"
stepN ∷ (m ∷ Nat) → InTime n α → InTime (m+n) α
stepN _ (IT x) = IT x
-}

{-| One-off typeclass. `Proxy` allows us to reflect a term-level value
  into the type level.
-}
class Step n c where
  stepN ∷ Proxy n → c m α → c (n+m) α


{-| Example usage:
  $ let v0 = 1 :# 2 :# Nil ∷ Vec 2 Int
  $ :t stepN (Proxy ∷ Proxy 10) (tReturn v0)
  > … ∷ (IT v0 ∷ InTime 10 (Vec 2 Int))
-}
instance Step n InTime where
  stepN _ (IT x) = IT x
