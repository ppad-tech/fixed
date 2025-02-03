{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Criterion.Main
import qualified Data.Bits as B
import qualified Data.Primitive.PrimArray as PA
import qualified Data.Word.Extended as W
import Data.Word (Word64)
import Prelude hiding (or, and, div, mod)
import qualified Prelude (div)

add_sub :: Benchmark
add_sub = bgroup "addition & subtraction" [
    add
  , add_baseline
  , sub
  , sub_baseline
  ]

multiplication :: Benchmark
multiplication = bgroup "multiplication" [
    mul
  , mul_baseline
  ]

division :: Benchmark
division = bgroup "division" [
    div
  , div_baseline
  , mod
  , mod_baseline
  , recip_2by1
  , quotrem_by1
  , rem_by1
  , quotrem_2by1
  , quot_r
  , quotrem_r
  ]

main :: IO ()
main = defaultMain [
    add_sub
  , multiplication
  , division
  ]

-- addition and subtraction ---------------------------------------------------

add_baseline :: Benchmark
add_baseline = bench "add (baseline)" $ nf ((+) w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

add :: Benchmark
add = bench "add" $ nf (W.add w0) w1 where
  !w0 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

sub_baseline :: Benchmark
sub_baseline = bench "sub (baseline)" $ nf ((-) w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

sub :: Benchmark
sub = bench "sub" $ nf (W.sub w0) w1 where
  !w0 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

-- multiplication -------------------------------------------------------------
mul_baseline :: Benchmark
mul_baseline = bench "mul (baseline)" $ nf ((*) w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

mul :: Benchmark
mul = bench "mul" $ nf (W.mul w0) w1 where
  !w0 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

-- division -------------------------------------------------------------------

quotrem_r :: Benchmark
quotrem_r = bench "quotrem_r" $
  nf (W.quotrem_r 4 0xffffffffffffffff) (B.complement 4)

quot_r :: Benchmark
quot_r = bench "quot_r" $
  nf (W.quot_r 4 0xffffffffffffffff) (B.complement 4)

recip_2by1 :: Benchmark
recip_2by1 = bench "recip_2by1" $
    nf W.recip_2by1 0xFFFF_FFFF_FFFF_FF00

quotrem_2by1 :: Benchmark
quotrem_2by1 = bench "quotrem_2by1" $
    nf (W.quotrem_2by1 8 4 0xFFFF_FFFF_FFFF_FF00) r
  where
    !r = W.recip_2by1 0xFFFF_FFFF_FFFF_FF00

quotrem_by1 :: Benchmark
quotrem_by1 = env setup $ \ ~(q, u, d) ->
    bench "quotrem_by1" $
      nfAppIO (W.quotrem_by1 q u) d
  where
    setup = do
      qm <- PA.newPrimArray 2
      PA.setPrimArray qm 0 2 0
      let !u = PA.primArrayFromList [4, 8]
          !d = B.complement 0xFF :: Word64
      pure (qm, u, d)

rem_by1 :: Benchmark
rem_by1 = bench "rem_by1" $
  nf (W.rem_by1 (PA.primArrayFromList [4, 8])) (B.complement 0xFF :: Word64)

div_baseline :: Benchmark
div_baseline = bench "div (baseline)" $ nf (Prelude.div w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x41cf50c7d0d65afabcf5ba37750dba71c7db29ec9f20a216d3ef013a59b9188a
  !w1 = 0x066bd4c3c10e30260cb6e7832af25f15527b089b258a1fef13b6eec3ce73bf06

div :: Benchmark
div = bench "div" $ nf (W.div w0) w1 where
  !w0 = W.to_word256
    0x41cf50c7d0d65afabcf5ba37750dba71c7db29ec9f20a216d3ef013a59b9188a
  !w1 = W.to_word256
    0x066bd4c3c10e30260cb6e7832af25f15527b089b258a1fef13b6eec3ce73bf06

mod_baseline :: Benchmark
mod_baseline = bench "mod (baseline)" $ nf (Prelude.rem w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x41cf50c7d0d65afabcf5ba37750dba71c7db29ec9f20a216d3ef013a59b9188a
  !w1 = 0x066bd4c3c10e30260cb6e7832af25f15527b089b258a1fef13b6eec3ce73bf06

mod :: Benchmark
mod = bench "mod" $ nf (W.mod w0) w1 where
  !w0 = W.to_word256
    0x41cf50c7d0d65afabcf5ba37750dba71c7db29ec9f20a216d3ef013a59b9188a
  !w1 = W.to_word256
    0x066bd4c3c10e30260cb6e7832af25f15527b089b258a1fef13b6eec3ce73bf06

