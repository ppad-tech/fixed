{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.DeepSeq
import qualified Data.Word.Extended as W
import qualified Weigh as W

instance NFData W.Word256
instance NFData W.Word512

i0, i1 :: Integer
i0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
i1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

w0, w1 :: W.Word256
w0 = W.to_word256 i0
w1 = W.to_word256 i1

i2, i3 :: Integer
i2 = 0x41cf50c7d0d65afabcf5ba37750dba71c7db29ec9f20a216d3ef013a59b9188a
i3 = 0x066bd4c3c10e30260cb6e7832af25f15527b089b258a1fef13b6eec3ce73bf06

w2, w3 :: W.Word256
w2 = W.to_word256 i2
w3 = W.to_word256 i3

main :: IO ()
main = W.mainWith $ do
  W.func "add (baseline)" ((+) i0) i1
  W.func "add" (W.add w0) w1
  W.func "sub (baseline)" ((-) i0) i1
  W.func "sub" (W.sub w0) w1
  W.func "mul (baseline)" ((-) i0) i1
  W.func "mul" (W.mul_512 w0) w1
  W.func "mul128 (baseline)" ((-) i0) i1
  W.func "mul128" (W.mul w0) w1
  W.func "div (baseline)" (Prelude.div i2) i3
  W.func "div" (W.div w2) w3

