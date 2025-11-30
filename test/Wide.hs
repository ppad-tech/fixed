{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Wide (
    tests
  ) where

import qualified Data.Choice as C
import qualified Data.Word.Wide as W
import GHC.Exts
import Test.Tasty
import qualified Test.Tasty.HUnit as H

overflowing_add_no_carry :: H.Assertion
overflowing_add_no_carry = do
  let !(r, c) = W.add_c 1 0
  H.assertBool mempty (W.eq_vartime r 1)
  H.assertBool mempty (c == 0)

overflowing_add_with_carry :: H.Assertion
overflowing_add_with_carry = do
  let !(r, c) = W.add_c (2 ^ (128 :: Word) - 1) 1
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

tests :: TestTree
tests = testGroup "wide tests" [
    H.testCase "overflowing add, no carry" overflowing_add_no_carry
  , H.testCase "overflowing add, carry" overflowing_add_with_carry
  , H.testCase "wrapping add, no carry" wrapping_add_no_carry
  , H.testCase "wrapping add, carry" wrapping_add_with_carry
  ]

