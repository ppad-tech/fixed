{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Limb as L
import Test.Tasty

main :: IO ()
main = defaultMain $ L.tests

