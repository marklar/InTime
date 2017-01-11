module Main where

import Vec

main :: IO ()
main =
  print $ (1 :# 2 :# Nil) +# (11 :# 12 :# Nil)
