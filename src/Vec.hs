{-# LANGUAGE DataKinds #-}           -- to use TypeLits in type sig
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}      -- to allow kinds (e.g. Type, Nat) in sig of GADT
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}       -- to use (>), (+), etc. in type sig
{-# LANGUAGE UnicodeSyntax #-}

-- Without OPTIONS_GHC, we get this type-checking error in `append`:
-- Could not deduce: ((m - 1) + n) ~ ((m + n) - 1)
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
 
module Vec where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

import Constraints


infixr 5 :#

{-| Different semantics?
  data Vec (n ∷ Nat) (α ∷ Type) ∷ Type where
  data Vec (n ∷ Nat) ∷ Type → Type where
  data Vec ∷ Nat → Type → Type where
-}
data Vec ∷ Nat → Type → Type where
  Nil  ∷ Vec 0 α
  (:#) ∷ α → Vec n α → Vec (n + 1) α

deriving instance Show α ⇒ Show (Vec n α)
deriving instance Eq α ⇒ Eq (Vec n α)


vhead ∷ (n > 0) ⇒ Vec n α → α
vhead (x :# _) = x


vtail ∷ (n > 0) ⇒ Vec n α → Vec (n - 1) α
vtail (_ :# xs) = xs


-- Necessary?
vlen ∷ Vec n α → Integer
vlen Nil = 0
vlen (x :# xs) = 1 + vlen xs


-- append
(+#) ∷ Vec m α → Vec n α → Vec (m + n) α
(+#) Nil       ys = ys
(+#) (x :# xs) ys = x :# (xs +# ys)


{-| Assumes: ys is sorted. (Or more generally, it inserts an element
into the Vec in such a way that if the input is sorted the output will
also be sorted.
-}
insert ∷ (Ord α) ⇒ α → Vec n α → Vec (n + 1) α
insert x Nil = x :# Nil
insert x (y :# ys)
  | x < y     = x :# (y :# ys)
  | otherwise = y :# (insert x ys)


-- (n ^ 2 + n) + (n + 1) ≡ (1 + n) ^ 2
-- (n+1) * (n+1)  =  n² + n + n + 1  =  n² + 2*n + 1

insertionSort ∷ (Ord α) ⇒ Vec n α → Vec n α
insertionSort Nil       = Nil
insertionSort (x :# xs) = insert x (insertionSort xs)


instance Functor (Vec n) where
  fmap _ Nil       = Nil
  fmap f (x :# xs) = (f x) :# fmap f xs


{-
-- Type-level fn for length of Vec. Unneeded.

class Length (n ∷ Nat) where
  vLen ∷ Vec n α → Proxy n

instance Length n where
  vLen xs = Proxy
-}
