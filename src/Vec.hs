{-# LANGUAGE DataKinds #-}           -- to use TypeLits in type sig
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}      -- to allow kinds (e.g. Type, Nat) in sig of GADT
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}       -- to use (>), (+), etc. in type sig
{-# LANGUAGE UndecidableInstances #-}
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
(+#) ∷ ∀ n m t α. (t ~ (m+n)) ⇒ Vec m α → Vec n α → Vec t α
(+#) Nil       ys = ys
(+#) (x :# xs) ys = x :# (xs +# ys)


{-| Assumes: ys is sorted. (Or more generally, it inserts an element
into the Vec in such a way that if the input is sorted the output will
also be sorted.
-}
insert ∷ (t ~ (n+1), Ord α) ⇒ α → Vec n α → Vec t α
insert x Nil = x :# Nil
insert x (y :# ys)
  | x < y     = x :# (y :# ys)
  | otherwise = y :# (insert x ys)


-- Make instance of Foldable, and implement w/ fold.
inSort ∷ (Ord α) ⇒ Vec n α → Vec n α
inSort Nil       = Nil
inSort (x :# xs) = insert x (inSort xs)


instance Functor (Vec n) where
  fmap _ Nil       = Nil
  fmap f (x :# xs) = (f x) :# fmap f xs
