{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Word.Extended as W
import Control.DeepSeq
import Criterion.Main

instance NFData W.Word256
instance NFData W.Word512

add_baseline :: Benchmark
add_baseline = bench "add (baseline)" $ nf ((+) w0) w1 where
  w0, w1 :: Integer
  w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

add :: Benchmark
add = bench "add" $ nf (W.add w0) w1 where
  w0 = W.to_word256
        0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  w1 = W.to_word256
        0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

sub_baseline :: Benchmark
sub_baseline = bench "sub (baseline)" $ nf ((-) w0) w1 where
  w0, w1 :: Integer
  w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

sub :: Benchmark
sub = bench "sub" $ nf (W.sub w0) w1 where
  w0 = W.to_word256
        0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  w1 = W.to_word256
        0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

mul_baseline :: Benchmark
mul_baseline = bench "mul (baseline)" $ nf ((*) w0) w1 where
  w0, w1 :: Integer
  w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

mul :: Benchmark
mul = bench "mul" $ nf (W.mul_512 w0) w1 where
  w0 = W.to_word256
        0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  w1 = W.to_word256
        0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed

mul128_baseline :: Benchmark
mul128_baseline = bench "mul128 (baseline)" $ nf ((*) w0) w1 where
  w0, w1 :: Integer
  w0 = 0x7fffffffffffffffffffffffffffffed
  w1 = 0x7ffffffffffffffbffffffffffffffed

mul128 :: Benchmark
mul128 = bench "mul128" $ nf (W.mul w0) w1 where
  w0 = W.to_word256 0x7fffffffffffffffffffffffffffffed
  w1 = W.to_word256 0x7ffffffffffffffbffffffffffffffed

main :: IO ()
main = defaultMain [
    add_baseline
  , add
  , sub_baseline
  , sub
  , mul_baseline
  , mul
  , mul128_baseline
  , mul128
  ]

