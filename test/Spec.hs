{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}

import Test.Hspec
import Vec

(≡≡) ∷ (Eq α, Show α) ⇒ α → α → IO ()
(≡≡) = shouldBe
infixr 1 ≡≡

main :: IO ()
main =
  hspec $ do

  describe "vhead" $ do
    -- NB: doesn't type-check: `vhead Nil`
    it "returns the head of a non-nil Vec" $ do
      vhead ('a' :# Nil) ≡≡ 'a'
      vhead ('a' :# 'b' :# 'c' :# Nil) ≡≡ 'a'
      vhead (1 :# 2 :# 3 :# Nil) ≡≡ 1

  describe "vtail" $ do
    -- NB: doesn't type-check: `vtail Nil`
    it "works with non-nil Vecs" $ do
      vtail ('a' :# Nil) ≡≡ Nil
      vtail ('a' :# 'b' :# Nil) ≡≡ 'b' :# Nil

  describe "append" $ do
    it "works when both Vecs are nil" $ do
      Nil +# Nil ≡≡ (Nil ∷ Vec 0 Int)
    it "works when one Vec is nil" $ do
      Nil +# (1 :# Nil) ≡≡ 1 :# Nil
      (1 :# Nil) +# Nil ≡≡ 1 :# Nil
    it "works with both Vecs non-nil" $ do
      (1 :# 2 :# Nil) +# (11 :# 12 :# Nil) ≡≡ 1 :# 2 :# 11 :# 12 :# Nil

  describe "insert" $ do
    it "works with nil Vec" $ do
      insert 1 Nil ≡≡ 1 :# Nil
    it "sticks LT val at front" $ do
      insert 1 (2 :# 3 :# Nil) ≡≡ 1 :# 2 :# 3 :# Nil
    it "inserts middling val in middle" $ do
      insert 2 (1 :# 3 :# Nil) ≡≡ 1 :# 2 :# 3 :# Nil
    it "puts GT val at end" $ do
      insert 3 (1 :# 2 :# Nil) ≡≡ 1 :# 2 :# 3 :# Nil
    it "includes both vals if EQ" $ do
      insert 2 (1 :# 2 :# Nil) ≡≡ 1 :# 2 :# 2 :# Nil

  describe "functor instance" $ do
    it "can be mapped over" $ do
      fmap (*3) (1 :# 2 :# 3 :# Nil) ≡≡ (3 :# 6 :# 9 :# Nil)
    
