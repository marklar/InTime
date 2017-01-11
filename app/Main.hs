{-# LANGUAGE DataKinds #-}              -- for: TypeLits (e.g. 0, 1) in type sig
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Vec
import InTime
import VecTime
import Data.Proxy


main ∷ IO ()
main = do
  print $ v0 +# v1
  print . tForce $ v0 +++ v1

  print (stepN (Proxy ∷ Proxy 10) (tReturn v1) ∷ InTime 10 (Vec 2 Int))
  print (step3 (tReturn v1) ∷ InTime 3 (Vec 2 Int))

  where
    v0 = 1  :# 2  :# Nil
    v1 = 11 :# 12 :# Nil
