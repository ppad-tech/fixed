{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Bits ((.|.), (.&.), (.^.))
import qualified Data.Bits as B
import qualified Data.Word.Extended as W
import qualified Data.Primitive.PrimArray as PA
import Prelude hiding (or, and, div, mod)
import qualified Prelude (div)

instance NFData W.Word256
instance NFData W.Word512
instance NFData W.Word256WithOverflow

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

div_baseline_small :: Benchmark
div_baseline_small =
    bench "div, small (baseline)" $ nf (Prelude.div w0) w1
  where
    w0, w1 :: Integer
    !w0 = 0x7fffffed
    !w1 = 0x7ffbffed

div_small :: Benchmark
div_small = bench "div, small" $ nf (W.div w0) w1 where
  !w0 = W.to_word256 0x7fffffed
  !w1 = W.to_word256 0x7ffbffed

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

quotrem_by1 :: Benchmark
quotrem_by1 = env setup $ \ ~(quo, u, d) ->
    bench "quotrem_by1" $ nfAppIO (W.quotrem_by1 quo u) d
  where
    setup = do
      quo <- PA.newPrimArray 5
      PA.setPrimArray quo 0 5 0
      let u = PA.primArrayFromList [
              300
            , 200
            , 100
            ]
          d = B.complement 50
      pure (quo, u, d)

quotrem_by1_256 :: Benchmark
quotrem_by1_256 =
  bench "quotrem_by1_256" $
    nf (W.quotrem_by1_256 (W.Word256 300 200 100 0)) (B.complement 50)

main :: IO ()
main = defaultMain [
    quotrem_by1
  , quotrem_by1_256
  --  mul_baseline
  --  mul
  --, div_baseline
  --, div
  --, mod_baseline
  --, mod
  --, div_baseline_small
  --, div_small
  --, or_baseline
  --, or
  --, and_baseline
  --, and
  --, xor_baseline
  --, xor
  --, add_baseline
  --, add
  --, sub_baseline
  --, sub
  ]

