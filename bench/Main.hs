{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits ((.|.), (.&.), (.^.))
import qualified Data.Word.Extended as W
import Control.DeepSeq
import Criterion.Main
import Prelude hiding (or, and, div)
import qualified Prelude (div)

instance NFData W.Word256
instance NFData W.Word512

or_baseline :: Benchmark
or_baseline = bench "or (baseline)" $ nf ((.|.) w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

or :: Benchmark
or = bench "or" $ nf (W.or w0) w1 where
  !w0 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

and_baseline :: Benchmark
and_baseline = bench "and (baseline)" $ nf ((.&.) w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

and :: Benchmark
and = bench "and" $ nf (W.and w0) w1 where
  !w0 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

xor_baseline :: Benchmark
xor_baseline = bench "xor (baseline)" $ nf ((.^.) w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

xor :: Benchmark
xor = bench "xor" $ nf (W.xor w0) w1 where
  !w0 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

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

mul_baseline :: Benchmark
mul_baseline = bench "mul (baseline)" $ nf ((*) w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

-- XX overflows; unsure if valid comparison
mul :: Benchmark
mul = bench "mul" $ nf (W.mul w0) w1 where
  !w0 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  !w1 = W.to_word256
    0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

mul128_baseline :: Benchmark
mul128_baseline = bench "mul128 (baseline)" $ nf ((*) w0) w1 where
  w0, w1 :: Integer
  !w0 = 0x7fffffffffffffffffffffffffffffed
  !w1 = 0x7ffffffffffffffbffffffffffffffed

mul128 :: Benchmark
mul128 = bench "mul128" $ nf (W.mul w0) w1 where
  !w0 = W.to_word256 0x7fffffffffffffffffffffffffffffed
  !w1 = W.to_word256 0x7ffffffffffffffbffffffffffffffed

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

main :: IO ()
main = defaultMain [
    or_baseline
  , or
  , and_baseline
  , and
  , xor_baseline
  , xor
  , add_baseline
  , add
  , sub_baseline
  , sub
  , mul_baseline
  , mul
  , mul128_baseline
  , mul128
  , div_baseline
  , div
  ]

