{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.Bits as B
import qualified Data.Word.Extended as E
import qualified Weigh as W

i0, i1 :: Integer
i0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
i1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

w0, w1 :: E.Word256
w0 = E.to_word256 i0
w1 = E.to_word256 i1

i2, i3 :: Integer
i2 = 0x41cf50c7d0d65afabcf5ba37750dba71c7db29ec9f20a216d3ef013a59b9188a
i3 = 0x066bd4c3c10e30260cb6e7832af25f15527b089b258a1fef13b6eec3ce73bf06

w2, w3 :: E.Word256
w2 = E.to_word256 i2
w3 = E.to_word256 i3

main :: IO ()
main = do
  let !u0 = E.Word576 300 200 100 0 0 0 0 0 0
      !u1 = E.Word576
        0x1234567890ABCDEF
        0xFEDCBA0987654321
        0x123456789ABCDEF0
        0 0 0 0 0 0

      !u2 = E.Word576
        2162362899639802732
        8848548347662387477
        13702897166684377657
        16799544643779908154
        1 0 0 0 0

      !d0 = B.complement 50

      !d1 = E.Word256 0x0 0x0 0x1 0x100000000

      !d2 = E.Word256
        16950798510782491100
        2612788699139816405
        5146719872810836952
        14966148379609982000

  W.mainWith $ do
    W.func "add (baseline)" ((+) i0) i1
    W.func "add" (E.add w0) w1
    W.func "sub (baseline)" ((-) i0) i1
    W.func "sub" (E.sub w0) w1
    W.func "mul (baseline)" ((*) i0) i1
    W.func "mul" (E.mul w0) w1
    W.func "div (baseline)" (Prelude.div i2) i3
    W.func "div" (E.div w2) w3
    W.func "quotrem_by1" (E.quotrem_by1 u0 3) d0
    W.func "quotrem" (E.quotrem u1) d1
    W.func "quotrem_knuth" (E.quotrem_knuth u2 5 d2) 4


