{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Limb as L
import Wide as W
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "ppad-fixed" [
    L.tests
  , W.tests
  ]

