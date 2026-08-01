{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Wide (
    tests
  ) where

import qualified Data.Choice as C
import qualified Data.Word.Wide as W
import Test.Tasty
import qualified Test.Tasty.HUnit as H

overflowing_add_no_carry :: H.Assertion
overflowing_add_no_carry = do
  let !(r, c) = W.add_o 1 0
  H.assertBool mempty (W.eq_vartime r 1)
  H.assertBool mempty (c == 0)

overflowing_add_with_carry :: H.Assertion
overflowing_add_with_carry = do
  let !(r, c) = W.add_o (2 ^ (128 :: Word) - 1) 1
  H.assertBool mempty (W.eq_vartime r 0)
  H.assertBool mempty (c == 1)

wrapping_add_no_carry :: H.Assertion
wrapping_add_no_carry = do
  let !r = W.add 0 1
  H.assertBool mempty (W.eq_vartime r 1)

wrapping_add_with_carry :: H.Assertion
wrapping_add_with_carry = do
  let !r = W.add (2 ^ (128 :: Word) - 1) 1
  H.assertBool mempty (W.eq_vartime r 0)

eq :: H.Assertion
eq = do
  let !a = 0 :: W.Wide
      !b = 2 ^ (128 :: Word) - 1
  H.assertBool mempty (C.decide (W.eq a a))
  H.assertBool mempty (not (C.decide (W.eq a b)))
  H.assertBool mempty (C.decide (W.eq b b))
  -- eq must yield a full-word mask, not a bare bit; negating or
  -- selecting on it is otherwise wrong
  H.assertBool mempty (not (C.decide (C.not (W.eq a a))))
  H.assertBool mempty (C.decide (C.not (W.eq a b)))
  H.assertBool mempty (W.eq_vartime (W.select a b (W.eq a a)) b)

tests :: TestTree
tests = testGroup "wide tests" [
    H.testCase "overflowing add, no carry" overflowing_add_no_carry
  , H.testCase "overflowing add, carry" overflowing_add_with_carry
  , H.testCase "wrapping add, no carry" wrapping_add_no_carry
  , H.testCase "wrapping add, carry" wrapping_add_with_carry
  , H.testCase "eq" eq
  ]

