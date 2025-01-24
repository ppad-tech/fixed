{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.DeepSeq
import qualified Data.Bits as B
import qualified Data.Primitive.PrimArray as PA
import qualified Data.Word.Extended as E
import qualified Weigh as W

instance NFData E.Word256
instance NFData E.Word320
instance NFData E.Word512
instance NFData E.Word576
instance NFData E.Word640

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
  !(!q, !u, !d) <- do
      quo <- PA.newPrimArray 5
      PA.setPrimArray quo 0 5 0
      let uf = PA.primArrayFromList [
              300
            , 200
            , 100
            ]
          df = B.complement 50
      pure (quo, uf, df)

  W.mainWith $ do
    W.func "add (baseline)" ((+) i0) i1
    W.func "add" (E.add w0) w1
    W.func "sub (baseline)" ((-) i0) i1
    W.func "sub" (E.sub w0) w1
    W.func "mul (baseline)" ((*) i0) i1
    W.func "mul" (E.mul w0) w1
    W.func "div (baseline)" (Prelude.div i2) i3
    W.func "div" (E.div w2) w3
    W.func "div_pure" (E.div_pure w2) w3
    W.io "quotrem_by1" (E.quotrem_by1 q u) d
    W.func "quotrem_by1_gen"
      (E.quotrem_by1_gen (E.Word576 300 200 100 0 0 0 0 0 0) 3) (B.complement 50)

