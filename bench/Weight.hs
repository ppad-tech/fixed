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
w0 = W.to_word256
  0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
w1 = W.to_word256
  0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

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

