{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
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

borrowing_sub_no_borrow :: H.Assertion
borrowing_sub_no_borrow = do
  let !(d, b) = W.sub_b 1 1
  H.assertBool mempty (W.eq_vartime d 0)
  H.assertBool mempty (b == 0)

borrowing_sub_with_borrow :: H.Assertion
borrowing_sub_with_borrow = do
  let !(d, b) = W.sub_b 0 1
  H.assertBool mempty (W.eq_vartime d (2 ^ (256 :: Word) - 1))
  H.assertBool mempty (b == (2 ^ (64 :: Word) - 1))

wrapping_sub_no_borrow :: H.Assertion
wrapping_sub_no_borrow = do
  let !r = W.sub 1 1
  H.assertBool mempty (W.eq_vartime r 0)

wrapping_sub_with_borrow :: H.Assertion
wrapping_sub_with_borrow = do
  let !r = W.sub 0 1
  H.assertBool mempty (W.eq_vartime r (2 ^ (256 :: Word) - 1))

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

sqr :: H.Assertion
sqr = do
  let !n = 2 ^ (256 :: Word) - 1
      !(l, h ) = W.sqr n
  H.assertBool mempty (W.eq_vartime l 1)
  H.assertBool mempty (W.eq_vartime h (n - 1))

mul :: H.Assertion
mul = do
  let !n = 2 ^ (256 :: Word) - 1
  H.assertBool mempty (W.eq_vartime (W.mul 0 n) 0)
  H.assertBool mempty (W.eq_vartime (W.mul n 0) 0)
  H.assertBool mempty (W.eq_vartime (W.mul n n) 1)
  H.assertBool mempty (W.eq_vartime (W.mul 1 n) n)

tests :: TestTree
tests = testGroup "wider tests" [
    H.testCase "overflowing add, no carry" overflowing_add_no_carry
  , H.testCase "overflowing add, carry" overflowing_add_with_carry
  , H.testCase "wrapping add, no carry" wrapping_add_no_carry
  , H.testCase "wrapping add, carry" wrapping_add_with_carry
  , H.testCase "borrowing sub, no borrow" borrowing_sub_no_borrow
  , H.testCase "borrowing sub, borrow" borrowing_sub_with_borrow
  , H.testCase "wrapping sub, no borrow" wrapping_sub_no_borrow
  , H.testCase "wrapping sub, borrow" wrapping_sub_with_borrow
  , H.testCase "eq" eq
  , H.testCase "gt" gt
  , H.testCase "lt" lt
  , H.testCase "cmp" cmp
  , H.testCase "sqr" sqr
  , H.testCase "mul" mul
  ]

