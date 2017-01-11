{-# LANGUAGE DataKinds #-}           -- to use TypeLits (e.g. 0, 1) in type sig
{-# LANGUAGE KindSignatures #-}      -- to allow kinds (e.g. Type, Nat) in sig of GADT
{-# LANGUAGE TypeOperators #-}       -- to use (>), (+), etc. in type sig
{-# LANGUAGE UnicodeSyntax #-}

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


tForce ∷ InTime n α → α
tForce (IT x) = x


------

tStep ∷ InTime n α → InTime (n + 1) α
tStep (IT x) = IT x


-- ~ "tStep . tReturn"
tStepRet ∷ α → InTime 1 α
tStepRet x = IT x
