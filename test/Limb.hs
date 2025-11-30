{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Limb (
    tests
  ) where

import qualified Data.Choice as C
import qualified Data.Word.Limb as L
import GHC.Exts
import Test.Tasty
import qualified Test.Tasty.HUnit as H

overflowing_add_no_carry :: H.Assertion
overflowing_add_no_carry = do
  let !(# r, c #) = L.add_o# (L.Limb 0##) (L.Limb 1##)
  H.assertBool mempty (L.eq_vartime# r (L.Limb 1##))
  H.assertBool mempty (L.eq_vartime# c (L.Limb 0##))

overflowing_add_with_carry :: H.Assertion
overflowing_add_with_carry = do
  let !(# r, c #) = L.add_o# (L.Limb (not# 0##)) (L.Limb 1##)
  H.assertBool mempty (L.eq_vartime# r (L.Limb 0##))
  H.assertBool mempty (L.eq_vartime# c (L.Limb 1##))

wrapping_add_no_carry :: H.Assertion
wrapping_add_no_carry = do
  let !r = L.add_w# (L.Limb 0##) (L.Limb 1##)
  H.assertBool mempty (L.eq_vartime# r (L.Limb 1##))

wrapping_add_with_carry :: H.Assertion
wrapping_add_with_carry = do
  let !r = L.add_w# (L.Limb (not# 0##)) (L.Limb 1##)
  H.assertBool mempty (L.eq_vartime# r (L.Limb 0##))

borrowing_sub_no_borrow :: H.Assertion
borrowing_sub_no_borrow = do
  let !(# r, c #) = L.sub_b# (L.Limb 1##) (L.Limb 1##) (L.Limb 0##)
  H.assertBool mempty (L.eq_vartime# r (L.Limb 0##))
  H.assertBool mempty (L.eq_vartime# c (L.Limb 0##))

borrowing_sub_with_borrow :: H.Assertion
borrowing_sub_with_borrow = do
  let !(# r, c #) = L.sub_b# (L.Limb 0##) (L.Limb 1##) (L.Limb 0##)
  H.assertBool mempty (L.eq_vartime# r (L.Limb (not# 0##)))
  H.assertBool mempty (L.eq_vartime# c (L.Limb (not# 0##)))

wrapping_sub_no_borrow :: H.Assertion
wrapping_sub_no_borrow = do
  let !r = L.sub_w# (L.Limb 1##) (L.Limb 1##)
  H.assertBool mempty (L.eq_vartime# r (L.Limb 0##))

wrapping_sub_with_borrow :: H.Assertion
wrapping_sub_with_borrow = do
  let !r = L.sub_w# (L.Limb 0##) (L.Limb 1##)
  H.assertBool mempty (L.eq_vartime# r (L.Limb (not# 0##)))

shl1 :: H.Assertion
shl1 = do
  let !r = L.shl# (L.Limb 1##) 1#
  H.assertBool mempty (L.eq_vartime# r (L.Limb 2##))

shl2 :: H.Assertion
shl2 = do
  let !r = L.shl# (L.Limb 1##) 2#
  H.assertBool mempty (L.eq_vartime# r (L.Limb 4##))

shr1 :: H.Assertion
shr1 = do
  let !r = L.shr# (L.Limb 2##) 1#
  H.assertBool mempty (L.eq_vartime# r (L.Limb 1##))

shr2 :: H.Assertion
shr2 = do
  let !r = L.shr# (L.Limb 16##) 2#
  H.assertBool mempty (L.eq_vartime# r (L.Limb 4##))

eq :: H.Assertion
eq = do
  let !a = L.Limb 0##
      !b = L.Limb (not# 0##)
  H.assertBool mempty (C.decide (L.eq# a a))
  H.assertBool mempty (not (C.decide (L.eq# a b)))
  H.assertBool mempty (not (C.decide (L.eq# b a)))
  H.assertBool mempty (C.decide (L.eq# b b))

gt :: H.Assertion
gt = do
  let !a = L.Limb 0##
      !b = L.Limb 1##
      !c = L.Limb (not# 0##)
  H.assertBool mempty (C.decide (L.gt# b a))
  H.assertBool mempty (C.decide (L.gt# c a))
  H.assertBool mempty (C.decide (L.gt# c b))

  H.assertBool mempty (not (C.decide (L.gt# a a)))
  H.assertBool mempty (not (C.decide (L.gt# b b)))
  H.assertBool mempty (not (C.decide (L.gt# c c)))

  H.assertBool mempty (not (C.decide (L.gt# a b)))
  H.assertBool mempty (not (C.decide (L.gt# a c)))
  H.assertBool mempty (not (C.decide (L.gt# b c)))

lt :: H.Assertion
lt = do
  let !a = L.Limb 0##
      !b = L.Limb 1##
      !c = L.Limb (not# 0##)
  H.assertBool mempty (C.decide (L.lt# a b))
  H.assertBool mempty (C.decide (L.lt# a c))
  H.assertBool mempty (C.decide (L.lt# b c))

  H.assertBool mempty (not (C.decide (L.lt# a a)))
  H.assertBool mempty (not (C.decide (L.lt# b b)))
  H.assertBool mempty (not (C.decide (L.lt# c c)))

  H.assertBool mempty (not (C.decide (L.lt# b a)))
  H.assertBool mempty (not (C.decide (L.lt# c a)))
  H.assertBool mempty (not (C.decide (L.lt# c b)))

cswap :: H.Assertion
cswap = do
  let !a = L.Limb (not# 0##)
      !b = L.Limb 0##
      !(# a0, b0 #) = L.cswap# a b (C.false# ())
  H.assertBool mempty (L.eq_vartime# a0 (L.Limb (not# 0##)))
  H.assertBool mempty (L.eq_vartime# b0 (L.Limb 0##))
  let !(# a1, b1 #) = L.cswap# a0 b0 (C.true# ())
  H.assertBool mempty (L.eq_vartime# a1 (L.Limb 0##))
  H.assertBool mempty (L.eq_vartime# b1 (L.Limb (not# 0##)))

tests :: TestTree
tests = testGroup "limb tests" [
    H.testCase "overflowing add, no carry" overflowing_add_no_carry
  , H.testCase "overflowing add, carry" overflowing_add_with_carry
  , H.testCase "wrapping add, no carry" wrapping_add_no_carry
  , H.testCase "wrapping add, carry" wrapping_add_with_carry
  , H.testCase "borrowing sub, no borrow" borrowing_sub_no_borrow
  , H.testCase "borrowing sub, borrow" borrowing_sub_with_borrow
  , H.testCase "wrapping sub, no borrow" wrapping_sub_no_borrow
  , H.testCase "wrapping sub, borrow" wrapping_sub_with_borrow
  , H.testCase "left shift (1)" shl1
  , H.testCase "left shift (2)" shl2
  , H.testCase "right shift (1)" shr1
  , H.testCase "right shift (2)" shr2
  , H.testCase "eq" eq
  , H.testCase "gt" gt
  , H.testCase "lt" lt
  , H.testCase "cswap" cswap
  ]

