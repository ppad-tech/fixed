{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import qualified Limb
import qualified Wide
import qualified Wider
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "ppad-fixed" [
    Limb.tests
  , Wide.tests
  , Wider.tests
  ]

