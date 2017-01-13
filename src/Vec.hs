{-# LANGUAGE DataKinds #-}           -- to use TypeLits in type sig
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
  data Vec ∷ Nat → Type → Type where
-}
data Vec (n ∷ Nat) (α ∷ Type) ∷ Type where
-- data Vec ∷ Nat → Type → Type where
  Nil  ∷ Vec 0 α

  -- (:#) ∷ α → Vec n α → Vec (n + 1) α
  (:#) ∷ ∀ n α. (KnownNat n) ⇒ α → Vec n α → Vec (n+1) α
  -- (:#) ∷ (t ~ (n+1), KnownNat n, KnownNat t) ⇒ α → Vec n α → Vec t α
  -- (:#) ∷ ∀ t n α. (t ~ (n+1), KnownNat t, KnownNat n) ⇒ α → Vec n α → Vec t α

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


{-
-- append
(+#) ∷ ∀ n m t α. (t ~ (m+n), KnownNat m, KnownNat n, KnownNat t) ⇒ Vec m α → Vec n α → Vec t α
(+#) Nil       ys = ys
(+#) (x :# xs) ys = x :# (xs +# ys)
-}


{-
class Appendable a b c | a b → c, a c → b, b c → a where
-- class Appendable a b c where
  vappend ∷ a → b → c

-- instance (t ~ (m+1), u ~ (n+1), v ~ (o+1), Appendable (Vec m α) (Vec n α) (Vec o α)) ⇒
--          Appendable (Vec t α) (Vec u α) (Vec v α)  where
instance Appendable (Vec 0 α) (Vec n α) (Vec n α) where
  vappend Nil ys = ys

instance (KnownNat m, m > 0, KnownNat n, KnownNat t, t ~ (m+n),
           Appendable (Vec (m-1) α) (Vec n α) (Vec (m-1+n) α)) ⇒
         Appendable (Vec m α) (Vec n α) (Vec t α) where
  vappend (x :# xs) ys = x :# (xs `vappend` ys)
-}  


{-
class Appendable m n where
  vappend ∷ Vec m α → Vec n α → Vec (m+n) α

instance ∀ n. Appendable 0 n where
  vappend Nil ys = ys

instance ∀ m n t. (m > 0, t ~ (m+1), Appendable m n) ⇒ Appendable t n where
  vappend (x :# xs) ys = x :# (xs `vappend` ys)
-}

{-| Assumes: ys is sorted. (Or more generally, it inserts an element
into the Vec in such a way that if the input is sorted the output will
also be sorted.
-}
insert ∷ (t ~ (n+1), KnownNat n, KnownNat t, Ord α) ⇒ α → Vec n α → Vec t α
insert x Nil = x :# Nil
insert x (y :# ys)
  | x < y     = x :# (y :# ys)
  | otherwise = y :# (insert x ys)


-- Make instance of Foldable, and implement w/ fold.
inSort ∷ (KnownNat n, Ord α) ⇒ Vec n α → Vec n α
inSort Nil       = Nil
inSort (x :# xs) = insert x (inSort xs)


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
