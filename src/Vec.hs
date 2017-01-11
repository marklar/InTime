{-# LANGUAGE DataKinds #-}           -- to use TypeLits in type sig
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
import GHC.TypeLits
import Constraints


infixr 5 :#

data Vec ∷ Nat → Type → Type where
  Nil  ∷ Vec 0 α
  (:#) ∷ α → Vec n α → Vec (n + 1) α

deriving instance Show α ⇒ Show (Vec n α)
deriving instance Eq α ⇒ Eq (Vec n α)


vhead ∷ (n > 0) ⇒ Vec n α → α
vhead (x :# _) = x


vtail ∷ (n > 0) ⇒ Vec n α → Vec (n - 1) α
vtail (_ :# xs) = xs


-- append
(+#) ∷ Vec m α → Vec n α → Vec (m + n) α
(+#) Nil       ys = ys
(+#) (x :# xs) ys = x :# (xs +# ys)


-- Assumes ys is sorted.
insert ∷ (Ord α) ⇒ α → Vec n α → Vec (n + 1) α
insert x Nil = x :# Nil
insert x (y :# ys)
  | x < y     = x :# (y :# ys)
  | otherwise = y :# (insert x ys)


instance Functor (Vec n) where
  fmap _ Nil       = Nil
  fmap f (x :# xs) = (f x) :# fmap f xs
