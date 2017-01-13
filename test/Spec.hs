{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

import Test.Hspec
import Vec
import InTime
import VecTime
import Data.Proxy
import GHC.TypeLits


(≡≡) ∷ (Eq α, Show α) ⇒ α → α → IO ()
(≡≡) = shouldBe
infixr 1 ≡≡

main :: IO ()
main =
  hspec $ do

  -- VecTime
  {-
  describe "VecTime.vtAppend" $ do
    it "appends two vecs" $ do
      let v0 = 1  :# 2  :# Nil ∷ Vec 2 Int
          v1 = 11 :# 12 :# Nil ∷ Vec 2 Int
        in
        v0 +++ v1 ≡≡ (IT (1 :# 2 :# 11 :# 12 :# Nil) ∷ InTime 5 (Vec 4 Int))
  -}

  describe "VecTime.vtInsert" $ do
      it "inserts into Nil vec" $ do
        2 +$+ Nil ≡≡ (IT (2 :# Nil) ∷ InTime 1 (Vec 1 Int))
      let v0 = 1 :# 2 :# Nil ∷ Vec 2 Int in
        it "inserts into vec w/ same complexity, regardless of where" $ do
          0 +$+ v0 ≡≡ (IT (0 :# 1 :# 2 :# Nil) ∷ InTime 3 (Vec 3 Int))
          1 +$+ v0 ≡≡ (IT (1 :# 1 :# 2 :# Nil) ∷ InTime 3 (Vec 3 Int))
          2 +$+ v0 ≡≡ (IT (1 :# 2 :# 2 :# Nil) ∷ InTime 3 (Vec 3 Int))
          3 +$+ v0 ≡≡ (IT (1 :# 2 :# 3 :# Nil) ∷ InTime 3 (Vec 3 Int))

  -- InTime
  describe "InTime.tStep1" $ do
    let v0 = 1 :# 2 :# Nil ∷ Vec 2 Int in
      it "adds N to complexity" $ do
        tStep1 (tReturn v0) ≡≡ (IT v0 ∷ InTime 1 (Vec 2 Int))

  describe "InTime.tStepN" $ do
    let v0 = 1 :# 2 :# Nil ∷ Vec 2 Int in
      it "adds N to complexity" $ do
        tStepN (Proxy ∷ Proxy 1) (tReturn v0) ≡≡ (IT v0 ∷ InTime 1 (Vec 2 Int))
        tStepN (Proxy ∷ Proxy 3) (tReturn v0) ≡≡ (IT v0 ∷ InTime 3 (Vec 2 Int))

  describe "InTime.tReturn1" $ do
    let v0 = 1 :# 2 :# Nil ∷ Vec 2 Int in
      it "wraps w/ complexity of 1" $ do
        tReturn1 v0 ≡≡ (IT v0 ∷ InTime 1 (Vec 2 Int))

  describe "InTime.tReturn" $ do
    it "returns wrapped w/ 0 steps" $ do
      tReturn 'a' ≡≡ (IT 'a' ∷ InTime 0 Char)
      tReturn 3.7 ≡≡ (IT 3.7 ∷ InTime 0 Double)

  describe "InTime.tReturnN" $ do
    it "returns wrapped w/ N steps" $ do
      tReturnN (Proxy ∷ Proxy 3) 'a' ≡≡ (IT 'a' ∷ InTime 3 Char)
      tReturnN (Proxy ∷ Proxy 8) 3.7 ≡≡ (IT 3.7 ∷ InTime 8 Double)

  describe "InTime.tForce" $ do
    it "extracts α from container" $ do
      tForce (tReturn 'a') ≡≡ 'a'
      tForce (tReturn 3.7) ≡≡ 3.7

  describe "Vec.vlen" $ do
    let v0 = 1 :# 2 :# Nil ∷ Vec 2 Int in
      it "finds length as Integer" $ do
        vlen Nil ≡≡ 0
        vlen v0 ≡≡ 2
        -- vlen (v0 +# v0) ≡≡ 4

  describe "Vec.vhead" $ do
    -- NB: doesn't type-check: `vhead Nil`
    it "returns the head of a non-nil Vec" $ do
      vhead ('a' :# Nil) ≡≡ 'a'
      vhead ('a' :# 'b' :# 'c' :# Nil) ≡≡ 'a'
      vhead (1 :# 2 :# 3 :# Nil) ≡≡ 1

  describe "Vec.vtail" $ do
    -- NB: doesn't type-check: `vtail Nil`
    it "works with non-nil Vecs" $ do
      vtail ('a' :# Nil) ≡≡ Nil
      vtail ('a' :# 'b' :# Nil) ≡≡ 'b' :# Nil

  {-
  describe "Vec.append" $ do
    it "works when both Vecs are nil" $ do
      Nil +# Nil ≡≡ (Nil ∷ Vec 0 Int)
    it "works when one Vec is nil" $ do
      Nil +# (1 :# Nil) ≡≡ 1 :# Nil
      (1 :# Nil) +# Nil ≡≡ 1 :# Nil
    it "works with both Vecs non-nil" $ do
      (1 :# 2 :# Nil) +# (11 :# 12 :# Nil) ≡≡ 1 :# 2 :# 11 :# 12 :# Nil
  -}

  describe "Vec.insert" $ do
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

  describe "Vec.inSort" $ do
    it "sorts lists, keeps size" $ do
      inSort (Nil ∷ Vec 0 Int) ≡≡ Nil
      inSort (3 :# 1 :# 2 :# Nil) ≡≡ 1 :# 2 :# 3 :# Nil

  describe "Vec functor instance" $ do
    it "can be mapped over" $ do
      fmap (*3) (1 :# 2 :# 3 :# Nil) ≡≡ (3 :# 6 :# 9 :# Nil)
