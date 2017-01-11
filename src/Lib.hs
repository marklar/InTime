{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Lib where

import GHC.TypeLits (Nat, type (+))
import Data.Kind

data Vec ∷ Nat → α → Type where

data InTime ∷ Nat → α → Type where

data α ≡ β ∷ Type where
-- data (α ≡ β) ∷ * where
  Refl ∷ a ≡ a

-- ∀ α β.
--  (f : α → β) {x y} → x ≡ y → f x ≡ f y
-- cong ∷ (f ∷ * → *) → x ≡ y → f x ≡ f y

-- cong ∷ (t ~ (* → *)) ⇒ (f ∷ t) → (x ≡ y) → ((f x) ≡ (f y))
-- type Unary α = 

-- cong ∷ -- (t ~ (Type → Type))
     -- ⇒ (f ∷ t)
--      → (x ≡ y)
--      → ((f x) ≡ (f y))
-- cong f refl = refl
  
-- cong ∷ (x ∷ Nat) → (y ∷ Nat) 

-- cong ∷ (f a ∷ Nat → Nat) → (x ≡ y) → ((f x) ≡ (f y))

-- cong ∷ (f ∷ Nat → Nat) → Nat -- (x ≡ y) → ((f x) ≡ (f y))
-- cong = undefined


-- type foo α β = 

f ∷ 6 ≡ (5+1)
f = Refl

{-
g ∷ ((+)1) ≡ ((+)1)
g = Refl
-}
-- map ∷
