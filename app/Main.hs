{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Vec
import InTime
import VecTime

main ∷ IO ()
main = do
  print $ v0 +# v1
  print . tForce $ v0 +++ v1
  where
    v0 = 1  :# 2  :# Nil
    v1 = 11 :# 12 :# Nil
