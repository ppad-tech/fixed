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
  -- , recip_2by1
  -- , quotrem_by1
  -- , rem_by1
  -- , quotrem_2by1
  -- , quot_r
  -- , quotrem_r
  ]

main :: IO ()
main = defaultMain [
    division
  , multiplication
  , add_sub
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


-- quotrem_by1_case0 :: H.Assertion
-- quotrem_by1_case0 = do
--   qm <- PA.newPrimArray 2
--   PA.setPrimArray qm 0 2 0
--   let !u = PA.primArrayFromList [4, 8]
--       !d = B.complement 0xFF :: Word64
--   r <- quotrem_by1 qm u d
--   q <- PA.unsafeFreezePrimArray qm
--   H.assertEqual "quotient" (PA.primArrayFromList [8, 0]) q
--   H.assertEqual "remainder" 2052 r


-- or_baseline :: Benchmark
-- or_baseline = bench "or (baseline)" $ nf ((.|.) w0) w1 where
--   w0, w1 :: Integer
--   !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
--   !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed
--
-- or :: Benchmark
-- or = bench "or" $ nf (W.or w0) w1 where
--   !w0 = W.to_word256
--     0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
--   !w1 = W.to_word256
--     0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed
--
-- and_baseline :: Benchmark
-- and_baseline = bench "and (baseline)" $ nf ((.&.) w0) w1 where
--   w0, w1 :: Integer
--   !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
--   !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed
--
-- and :: Benchmark
-- and = bench "and" $ nf (W.and w0) w1 where
--   !w0 = W.to_word256
--     0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
--   !w1 = W.to_word256
--     0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed
--
-- xor_baseline :: Benchmark
-- xor_baseline = bench "xor (baseline)" $ nf ((.^.) w0) w1 where
--   w0, w1 :: Integer
--   !w0 = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
--   !w1 = 0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed
--
-- xor :: Benchmark
-- xor = bench "xor" $ nf (W.xor w0) w1 where
--   !w0 = W.to_word256
--     0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
--   !w1 = W.to_word256
--     0x7fffffffffffffffffffffffffffffffffffffffffffffbfffffffffffffffed
--
-- mul128_baseline :: Benchmark
-- mul128_baseline = bench "mul128 (baseline)" $ nf ((*) w0) w1 where
--   w0, w1 :: Integer
--   !w0 = 0x7fffffffffffffffffffffffffffffed
--   !w1 = 0x7ffffffffffffffbffffffffffffffed
--
-- mul128 :: Benchmark
-- mul128 = bench "mul128" $ nf (W.mul w0) w1 where
--   !w0 = W.to_word256 0x7fffffffffffffffffffffffffffffed
--   !w1 = W.to_word256 0x7ffffffffffffffbffffffffffffffed

-- arithmetic :: Benchmark
-- arithmetic = bgroup "arithmetic" [
--     add
--   , sub
--   , mul
--   , div
--   , mod
--   ]
--
-- baseline_arithmetic :: Benchmark
-- baseline_arithmetic = bgroup "baseline arithmetic" [
--     add_baseline
--   , sub_baseline
--   , mul_baseline
--   , div_baseline
--   , mod_baseline
--   ]
--
-- baseline_comparison :: Benchmark
-- baseline_comparison = bgroup "baseline comparison" [
--     add_baseline
--   , add
--   , sub_baseline
--   , sub
--   , mul_baseline
--   , mul
--   , div_baseline
--   , div
--   ]
--
-- bits :: Benchmark
-- bits = bgroup "bits" [
--     and
--   , or
--   , xor
--   ]
--
