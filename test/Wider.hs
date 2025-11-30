{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Wider (
    tests
  ) where

import qualified Data.Choice as C
import qualified Data.Word.Wider as W
import Test.Tasty
import qualified Test.Tasty.HUnit as H

overflowing_add_no_carry :: H.Assertion
overflowing_add_no_carry = do
  let !(r, c) = W.add_o 1 0
  H.assertBool mempty (W.eq_vartime r 1)
  H.assertBool mempty (c == 0)

overflowing_add_with_carry :: H.Assertion
overflowing_add_with_carry = do
  let !(r, c) = W.add_o (2 ^ (256 :: Word) - 1) 1
  H.assertBool mempty (W.eq_vartime r 0)
  H.assertBool mempty (c == 1)

wrapping_add_no_carry :: H.Assertion
wrapping_add_no_carry = do
  let !r = W.add 0 1
  H.assertBool mempty (W.eq_vartime r 1)

wrapping_add_with_carry :: H.Assertion
wrapping_add_with_carry = do
  let !r = W.add (2 ^ (256 :: Word) - 1) 1
  H.assertBool mempty (W.eq_vartime r 0)

eq :: H.Assertion
eq = do
  let !(W.Wider a) = 0
      !(W.Wider b) = 2 ^ (256 :: Word) - 1
  H.assertBool mempty (C.decide (W.eq# a a))
  H.assertBool mempty (not (C.decide (W.eq# a b)))
  H.assertBool mempty (not (C.decide (W.eq# b a)))
  H.assertBool mempty (C.decide (W.eq# b b))

gt :: H.Assertion
gt = do
  let !(W.Wider a) = 0
      !(W.Wider b) = 1
      !(W.Wider c) = 2 ^ (256 :: Word) - 1
  H.assertBool mempty (C.decide (W.gt# b a))
  H.assertBool mempty (C.decide (W.gt# c a))
  H.assertBool mempty (C.decide (W.gt# c b))

  H.assertBool mempty (not (C.decide (W.gt# a a)))
  H.assertBool mempty (not (C.decide (W.gt# b b)))
  H.assertBool mempty (not (C.decide (W.gt# c c)))

  H.assertBool mempty (not (C.decide (W.gt# a b)))
  H.assertBool mempty (not (C.decide (W.gt# a c)))
  H.assertBool mempty (not (C.decide (W.gt# b c)))

lt :: H.Assertion
lt = do
  let !(W.Wider a) = 0
      !(W.Wider b) = 1
      !(W.Wider c) = 2 ^ (256 :: Word) - 1
  H.assertBool mempty (C.decide (W.lt# a b))
  H.assertBool mempty (C.decide (W.lt# a c))
  H.assertBool mempty (C.decide (W.lt# b c))

  H.assertBool mempty (not (C.decide (W.lt# a a)))
  H.assertBool mempty (not (C.decide (W.lt# b b)))
  H.assertBool mempty (not (C.decide (W.lt# c c)))

  H.assertBool mempty (not (C.decide (W.lt# b a)))
  H.assertBool mempty (not (C.decide (W.lt# c a)))
  H.assertBool mempty (not (C.decide (W.lt# c b)))

cmp :: H.Assertion
cmp = do
  let !a = 0
      !b = 1
      !c = 2 ^ (256 :: Word) - 1
  H.assertEqual mempty (W.cmp a b) LT
  H.assertEqual mempty (W.cmp a c) LT
  H.assertEqual mempty (W.cmp b c) LT

  H.assertEqual mempty (W.cmp a a) EQ
  H.assertEqual mempty (W.cmp b b) EQ
  H.assertEqual mempty (W.cmp c c) EQ

  H.assertEqual mempty (W.cmp b a) GT
  H.assertEqual mempty (W.cmp c a) GT
  H.assertEqual mempty (W.cmp c b) GT

tests :: TestTree
tests = testGroup "wider tests" [
    H.testCase "overflowing add, no carry" overflowing_add_no_carry
  , H.testCase "overflowing add, carry" overflowing_add_with_carry
  , H.testCase "wrapping add, no carry" wrapping_add_no_carry
  , H.testCase "wrapping add, carry" wrapping_add_with_carry
  , H.testCase "eq" eq
  , H.testCase "gt" gt
  , H.testCase "lt" lt
  , H.testCase "cmp" cmp
  ]


