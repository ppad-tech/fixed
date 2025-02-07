{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.Word.Extended
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Large fixed-width words, complete with support for conversion,
-- comparison, bitwise operations, arithmetic, and modular arithmetic.

module Data.Word.Extended (
    Word256(..)
  , zero
  , one

  -- * Conversion
  , to_integer
  , to_word256

  -- * Comparison
  , lt
  , gt
  , is_zero

  -- * Bit Operations
  , or
  , and
  , xor

  -- * Arithmetic
  , add
  , sub
  , mul
  , div
  , mod

  -- for testing/benchmarking
  , Word128(..)
  , quotrem
  , quot_r
  , quotrem_r
  , quotrem_by1
  , rem_by1
  , quotrem_2by1
  , quotrem_knuth
  , recip_2by1
  , mul_c
  , mul_c#
  , umul_hop#
  , umul_step#
  , mul_512#
  ) where

import Control.DeepSeq
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.), (.^.))
import qualified Data.Bits as B
import qualified Data.Primitive.PrimArray as PA
import GHC.Exts
import GHC.Generics
import GHC.Word
import Prelude hiding (div, mod, or, and, quot, rem)
import qualified Prelude (quot, rem)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- word256 --------------------------------------------------------------------

-- | Little-endian Word256.
data Word256 = Word256
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Generic)

instance NFData Word256

instance Show Word256 where
  show = show . to_integer

-- utility words ------------------------------------------------------------

data Word128 = P
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic)

instance NFData Word128

-- conversion -----------------------------------------------------------------

-- | Convert a fixed-width 'Word256' into a variable-length 'Integer'.
--
--   >>> let foo = to_integer (Word256 0x1 0x10 0x100 0x1000)
--   >>> foo
--   25711008708143844408758505763390361887002166947932397379780609
to_integer :: Word256 -> Integer
to_integer (Word256 w0 w1 w2 w3) =
      fi w3 .<<. 192
  .|. fi w2 .<<. 128
  .|. fi w1 .<<. 64
  .|. fi w0

-- | Convert a fixed-width 'Word256' into a variable-length 'Integer'.
--
--   >>> (\(Word256 l _ _ _) -> l) (to_word256 foo)
--   1
to_word256 :: Integer -> Word256
to_word256 n =
  let !mask64 = 2 ^ (64 :: Int) - 1
      !w0 = fi (n .&. mask64)
      !w1 = fi ((n .>>. 64) .&. mask64)
      !w2 = fi ((n .>>. 128) .&. mask64)
      !w3 = fi ((n .>>. 192) .&. mask64)
  in  Word256 w0 w1 w2 w3

-- comparison -----------------------------------------------------------------

-- | Strict less-than comparison on 'Word256' values.
--
--   >>> to_word256 0 `lt` to_word256 1
--   True
--   >>> to_word256 0 `lt` to_word256 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
--   True
lt :: Word256 -> Word256 -> Bool
lt (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(P _ c0) = sub_b a0 b0 0
      !(P _ c1) = sub_b a1 b1 c0
      !(P _ c2) = sub_b a2 b2 c1
      !(P _ c3) = sub_b a3 b3 c2
  in  c3 /= 0

-- | Strict greater-than comparison on 'Word256' values.
--
--   >>> to_word256 0 `gt` to_word256 1
--   False
--   >>> to_word256 0 `gt` to_word256 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
--   False
gt :: Word256 -> Word256 -> Bool
gt a b = lt b a

-- | Zero, as a 'Word256'.
zero :: Word256
zero = Word256 0 0 0 0

-- | One, as a 'Word256'.
one :: Word256
one = Word256 1 0 0 0

-- | Test if a 'Word256' value is zero.
is_zero :: Word256 -> Bool
is_zero w = w == zero

is_word64 :: Word256 -> Bool
is_word64 (Word256 _ a b c) = a == 0 && b == 0 && c == 0

-- bits -----------------------------------------------------------------------

-- | Bitwise-or on 'Word256' values.
or :: Word256 -> Word256 -> Word256
or (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  Word256 (a0 .|. b0) (a1 .|. b1) (a2 .|. b2) (a3 .|. b3)

-- | Bitwise-and on 'Word256' values.
and :: Word256 -> Word256 -> Word256
and (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  Word256 (a0 .&. b0) (a1 .&. b1) (a2 .&. b2) (a3 .&. b3)

-- | Bitwise-xor on 'Word256' values.
xor :: Word256 -> Word256 -> Word256
xor (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  Word256 (a0 .^. b0) (a1 .^. b1) (a2 .^. b2) (a3 .^. b3)

-- addition, subtraction ------------------------------------------------------

-- add-with-carry
add_c :: Word64 -> Word64 -> Word64 -> Word128
add_c (W64# a) (W64# b) (W64# c) =
  let !(# s, n #) = add_c# a b c
  in  P (W64# s) (W64# n)

add_c# :: Word64# -> Word64# -> Word64# -> (# Word64#, Word64# #)
add_c# w64_0 w64_1 c =
  let !s = plusWord64# (plusWord64# w64_0 w64_1) c
      !n | isTrue# (orI# (ltWord64# s w64_0) (ltWord64# s w64_1)) = 1#
         | otherwise = 0#
  in  (# s, wordToWord64# (int2Word# n) #)
{-# INLINE add_c# #-}

-- add with overflow
add_of#
  :: (# Word64#, Word64#, Word64#, Word64# #)
  -> (# Word64#, Word64#, Word64#, Word64# #)
  -> (# Word64#, Word64#, Word64#, Word64#, Word64# #)
add_of# (# a0, a1, a2, a3 #)
        (# b0, b1, b2, b3 #) =
  let !(# s0, c0 #) = add_c# a0 b0 (wordToWord64# 0##)
      !(# s1, c1 #) = add_c# a1 b1 c0
      !(# s2, c2 #) = add_c# a2 b2 c1
      !(# s3, c3 #) = add_c# a3 b3 c2
  in  (# s0, s1, s2, s3, c3 #)
{-# INLINE add_of# #-}

-- | Addition on 'Word256' values, with overflow.
--
--   >>> to_word256 0xFFFFFFFFFF `add` to_word256 0xFFFFFF
--   18446742974181146625
add :: Word256 -> Word256 -> Word256
add (Word256 (W64# a0) (W64# a1) (W64# a2) (W64# a3))
     (Word256 (W64# b0) (W64# b1) (W64# b2) (W64# b3)) =
  let !(# c0, c1, c2, c3, _ #) = add_of#
        (# a0, a1, a2, a3 #)
        (# b0, b1, b2, b3 #)
  in  Word256 (W64# c0) (W64# c1) (W64# c2) (W64# c3)

-- subtract-with-borrow
sub_b :: Word64 -> Word64 -> Word64 -> Word128
sub_b (W64# wa) (W64# wb) (W64# b) =
  let !(# d, n #) = sub_b# wa wb b
  in  P (W64# d) (W64# n)

sub_b# :: Word64# -> Word64# -> Word64# -> (# Word64#, Word64# #)
sub_b# w64_0 w64_1 b =
  let !d = subWord64# (subWord64# w64_0 w64_1) b
      !n | isTrue# (ltWord64# w64_0 (plusWord64# w64_1 b)) = wordToWord64# 1##
         | otherwise = wordToWord64# 0##
  in  (# d, n #)
{-# INLINE sub_b# #-}

-- subtract-with-overflow
sub_of#
  :: (# Word64#, Word64#, Word64#, Word64# #)
  -> (# Word64#, Word64#, Word64#, Word64# #)
  -> (# Word64#, Word64#, Word64#, Word64#, Word64# #)
sub_of# (# a0, a1, a2, a3 #)
        (# b0, b1, b2, b3 #) =
  let !(# s0, c0 #) = sub_b# a0 b0 (wordToWord64# 0##)
      !(# s1, c1 #) = sub_b# a1 b1 c0
      !(# s2, c2 #) = sub_b# a2 b2 c1
      !(# s3, c3 #) = sub_b# a3 b3 c2
  in  (# s0, s1, s2, s3, c3 #)
{-# INLINE sub_of# #-}

-- | Subtraction on 'Word256' values.
--
--   >>> to_word256 0xFFFFFFFFFF `sub` to_word256 0xFFFFFF
--   1099494850560
sub :: Word256 -> Word256 -> Word256
sub (Word256 (W64# a0) (W64# a1) (W64# a2) (W64# a3))
     (Word256 (W64# b0) (W64# b1) (W64# b2) (W64# b3)) =
  let !(# c0, c1, c2, c3, _ #) = sub_of#
        (# a0, a1, a2, a3 #)
        (# b0, b1, b2, b3 #)
  in  Word256 (W64# c0) (W64# c1) (W64# c2) (W64# c3)

-- multiplication -------------------------------------------------------------

-- multiply-with-carry
mul_c :: Word64 -> Word64 -> Word128
mul_c (W64# x) (W64# y) =
  let !(# hi, lo #) = mul_c# x y
  in  P (W64# hi) (W64# lo)

-- translated from Mul64 in go's math/bits package
mul_c# :: Word64# -> Word64# -> (# Word64#, Word64# #)
mul_c# x y =
  let !mask32 = wordToWord64# 0xffffffff##
      !x0 = and64# x mask32
      !y0 = and64# y mask32
      !x1 = uncheckedShiftRL64# x 32#
      !y1 = uncheckedShiftRL64# y 32#

      !w0   = timesWord64# x0 y0
      !t    = plusWord64# (timesWord64# x1 y0) (uncheckedShiftRL64# w0 32#)
      !w1   = and64# t mask32
      !w2   = uncheckedShiftRL64# t 32#
      !w1_1 = plusWord64# w1 (timesWord64# x0 y1)

      !hi = plusWord64#
        (timesWord64# x1 y1)
        (plusWord64# w2 (uncheckedShiftRL64# w1_1 32#))
      !lo = timesWord64# x y
  in  (# hi, lo #)
{-# INLINE mul_c# #-}

umul_hop# :: Word64# -> Word64# -> Word64# -> (# Word64#, Word64# #)
umul_hop# z x y =
  let !(# hi_0, lo_0 #) = mul_c# x y
      !(# lo, c #)      = add_c# lo_0 z (wordToWord64# 0##)
      !(# hi, _ #)      = add_c# hi_0 (wordToWord64# 0##) c
  in  (# hi, lo #)
{-# INLINE umul_hop# #-}

umul_step#
  :: Word64#
  -> Word64#
  -> Word64#
  -> Word64#
  -> (# Word64#, Word64# #)
umul_step# z x y c =
  let !(# hi_0, lo_0 #) = mul_c# x y
      !(# lo_1, c_0 #)  = add_c# lo_0 c (wordToWord64# 0##)
      !(# hi_1, _ #)    = add_c# hi_0 (wordToWord64# 0##) c_0
      !(# lo, c_1 #)    = add_c# lo_1 z (wordToWord64# 0##)
      !(# hi, _ #)      = add_c# hi_1 (wordToWord64# 0##) c_1
  in  (# hi, lo #)
{-# INLINE umul_step# #-}

-- | Multiplication on 'Word256' values, with overflow.
--
--   >>> to_word256 0xFFFFFFFFFF `mul` to_word256 0xFFFFFF
--   18446742974181146625
mul :: Word256 -> Word256 -> Word256
mul (Word256 (W64# a0) (W64# a1) (W64# a2) (W64# a3))
    (Word256 (W64# b0) (W64# b1) (W64# b2) (W64# b3)) =
  let !(# c0_0, s0 #) = mul_c# a0 b0
      !(# c0_1, r0 #) = umul_hop# c0_0 a1 b0
      !(# c0_2, r1 #) = umul_hop# c0_1 a2 b0
      !(# c1_0, s1 #) = umul_hop# r0 a0 b1
      !(# c1_1, r2 #) = umul_step# r1 a1 b1 c1_0
      !(# c2, s2 #)   = umul_hop# r2 a1 b1
      !s3 = plusWord64# (timesWord64# a3 b0)
             (plusWord64# (timesWord64# a2 b1)
               (plusWord64# (timesWord64# a0 b3)
                 (plusWord64# (timesWord64# a1 b2)
                   (plusWord64# c0_2 (plusWord64# c1_1 c2)))))
  in  Word256 (W64# s0) (W64# s1) (W64# s2) (W64# s3)

-- full 256-bit x 256-bit -> 512-bit multiplication
mul_512#
  :: (# Word64#, Word64#, Word64#, Word64# #)
  -> (# Word64#, Word64#, Word64#, Word64# #)
  -> (# Word64#, Word64#, Word64#, Word64#, Word64#, Word64#, Word64#, Word64# #)
mul_512# (# x0, x1, x2, x3 #) (# y0, y1, y2, y3 #) =
  let !(# c4_0,   r0 #) = mul_c#    x0 y0
      !(# c4_1, r0_1 #) = umul_hop# c4_0 x1 y0
      !(# c4_2, r0_2 #) = umul_hop# c4_1 x2 y0
      !(# c4,   r0_3 #) = umul_hop# c4_2 x3 y0

      !(# c5_0,   r1 #) = umul_hop#  r0_1 x0 y1
      !(# c5_1, r1_2 #) = umul_step# r0_2 x1 y1 c5_0
      !(# c5_2, r1_3 #) = umul_step# r0_3 x2 y1 c5_1
      !(# c5,   r1_4 #) = umul_step# c4 x3 y1 c5_2

      !(# c6_0,   r2 #) = umul_hop#  r1_2 x0 y2
      !(# c6_1, r2_3 #) = umul_step# r1_3 x1 y2 c6_0
      !(# c6_2, r2_4 #) = umul_step# r1_4 x2 y2 c6_1
      !(# c6,   r2_5 #) = umul_step# c5 x3 y2 c6_2

      !(# c7_0,   r3 #) = umul_hop#  r2_3 x0 y3
      !(# c7_1,   r4 #) = umul_step# r2_4 x1 y3 c7_0
      !(# c7_2,   r5 #) = umul_step# r2_5 x2 y3 c7_1
      !(# r7,     r6 #) = umul_step# c6 x3 y3 c7_2
  in  (# r0, r1, r2, r3, r4, r5, r6, r7 #)

-- division -------------------------------------------------------------------

-- quotient, remainder of (hi, lo) divided by y
quotrem_r :: Word64 -> Word64 -> Word64 -> Word128
quotrem_r (W64# hi) (W64# lo) (W64# y) =
  let !(# q, r #) = quotrem_r# hi lo y
  in  P (W64# q) (W64# r)

-- translated from Div64 in go's math/bits package
quotrem_r# :: Word64# -> Word64# -> Word64# -> (# Word64#, Word64# #)
quotrem_r# hi lo y_0
    | isTrue# (eqWord64# y_0 (wordToWord64# 0##)) =
        error "ppad-fixed (quotrem_r): division by zero"
    | isTrue# (leWord64# y_0 hi) =
        error "ppad-fixed: overflow"
    | isTrue# (eqWord64# hi (wordToWord64# 0##)) =
        (# quotWord64# lo y_0, remWord64# lo y_0 #)
    | otherwise =
        let !s = int64ToInt# (word64ToInt64# (wordToWord64# (clz64# y_0)))
            !y = uncheckedShiftL64# y_0 s

            !yn1  = uncheckedShiftRL64# y 32#
            !yn0  = and64# y mask32
            !un32 = or64#
              (uncheckedShiftL64# hi s)
              (if   (isTrue# (s ==# 0#))
               then wordToWord64# 0##
               else uncheckedShiftRL64# lo (64# -# s))

            !un10 = uncheckedShiftL64# lo s
            !un1 = uncheckedShiftRL64# un10 32#
            !un0 = and64# un10 mask32
            !q1 = quotWord64# un32 yn1
            !rhat = subWord64# un32 (timesWord64# q1 yn1)

            !q1_l = q_loop# q1 rhat yn0 yn1 un1

            !un21 = subWord64#
              (plusWord64# (timesWord64# un32 two32) un1)
              (timesWord64# q1_l y)
            !q0 = quotWord64# un21 yn1
            !rhat_n = subWord64# un21 (timesWord64# q0 yn1)

            !q0_l = q_loop# q0 rhat_n yn0 yn1 un0

            !q = plusWord64# (timesWord64# q1_l two32) q0_l
            !r = uncheckedShiftRL64#
              (subWord64#
                (plusWord64# (timesWord64# un21 two32) un0)
                (timesWord64# q0_l y))
              s
        in  (# q, r #)
  where
    !two32  = wordToWord64# 0x100000000##
    !mask32 = wordToWord64# 0x0ffffffff##

    q_loop# !q_acc !rhat_acc !yn_0 !yn_1 !un =
      let go# !qa !rha
            | isTrue# (orI#
                (geWord64# qa two32)
                (gtWord64#
                  (timesWord64# qa yn_0)
                  (plusWord64# (timesWord64# two32 rha) un))) =
                let !qn = subWord64# qa (wordToWord64# 1##)
                    !rhn = plusWord64# rha yn_1
                in  if   isTrue# (geWord64# rhn two32)
                    then qn
                    else go# qn rhn
            | otherwise = qa
      in  go# q_acc rhat_acc
    {-# INLINE q_loop# #-}
{-# INLINE quotrem_r# #-}

-- same as quotrem_r, except only computes quotient
quot_r :: Word64 -> Word64 -> Word64 -> Word64
quot_r (W64# hi) (W64# lo) (W64# y) =
  let !q = quot_r# hi lo y
  in  W64# q

quot_r# :: Word64# -> Word64# -> Word64# -> Word64#
quot_r# hi lo y_0
    | isTrue# (eqWord64# y_0 (wordToWord64# 0##)) =
        error "ppad-fixed (quot_r): division by zero"
    | isTrue# (leWord64# y_0 hi) =
        error "ppad-fixed: overflow"
    | isTrue# (eqWord64# hi (wordToWord64# 0##)) =
        quotWord64# lo y_0
    | otherwise =
        let !s = int64ToInt# (word64ToInt64# (wordToWord64# (clz64# y_0)))
            !y = uncheckedShiftL64# y_0 s

            !yn1  = uncheckedShiftRL64# y 32#
            !yn0  = and64# y mask32
            !un32 = or64#
              (uncheckedShiftL64# hi s)
              (if   (isTrue# (s ==# 0#))
               then wordToWord64# 0##
               else uncheckedShiftRL64# lo (64# -# s))
            !un10 = uncheckedShiftL64# lo s
            !un1 = uncheckedShiftRL64# un10 32#
            !un0 = and64# un10 mask32
            !q1 = quotWord64# un32 yn1
            !rhat = subWord64# un32 (timesWord64# q1 yn1)

            !q1_l = q_loop# q1 rhat yn0 yn1 un1

            !un21 = subWord64#
              (plusWord64# (timesWord64# un32 two32) un1)
              (timesWord64# q1_l y)
            !q0 = quotWord64# un21 yn1
            !rhat_n = subWord64# un21 (timesWord64# q0 yn1)

            !q0_l = q_loop# q0 rhat_n yn0 yn1 un0

        in  plusWord64# (timesWord64# q1_l two32) q0_l
  where
    !two32  = wordToWord64# 0x100000000##
    !mask32 = wordToWord64# 0x0ffffffff##

    q_loop# !q_acc !rhat_acc !yn_0 !yn_1 !un =
      let go# !qa !rha
            | isTrue# (orI#
                (geWord64# qa two32)
                (gtWord64#
                  (timesWord64# qa yn_0)
                  (plusWord64# (timesWord64# two32 rha) un))) =
                let !qn = subWord64# qa (wordToWord64# 1##)
                    !rhn = plusWord64# rha yn_1
                in  if   isTrue# (geWord64# rhn two32)
                    then qn
                    else go# qn rhn
            | otherwise = qa
      in  go# q_acc rhat_acc
    {-# INLINE q_loop# #-}
{-# INLINE quot_r# #-}

-- XX these 2by1 names suck

-- quotient and remainder of (hi, lo) divided by d, using reciprocal
quotrem_2by1 :: Word64 -> Word64 -> Word64 -> Word64 -> Word128
quotrem_2by1 (W64# uh) (W64# ul) (W64# d) (W64# rec) =
  let !(# q, r #) = quotrem_2by1# uh ul d rec
  in  P (W64# q) (W64# r)

quotrem_2by1#
  :: Word64# -> Word64# -> Word64# -> Word64# -> (# Word64#, Word64# #)
quotrem_2by1# uh ul d rec =
  let !(# qh_0, ql #)  = mul_c# rec uh
      !(# ql_0, c #)   = add_c# ql ul (wordToWord64# 0##)
      !(# qh_1_l, _ #) = add_c# qh_0 uh c
      !qh_1            = plusWord64# qh_1_l (wordToWord64# 1##)
      !r               = subWord64# ul (timesWord64# qh_1 d)

      !(# qh_y, r_y #)
        | isTrue# (geWord64# r ql_0)  = (# qh_1_l, plusWord64# r d #)
        | otherwise = (# qh_1, r #)

  in  if   isTrue# (geWord64# r_y d)
      then (# plusWord64# qh_y (wordToWord64# 1##), subWord64# r_y d #)
      else (# qh_y, r_y #)
{-# INLINE quotrem_2by1# #-}

recip_2by1 :: Word64 -> Word64
recip_2by1 (W64# d) = W64# (recip_2by1# d)

recip_2by1# :: Word64# -> Word64#
recip_2by1# d = quot_r# (not64# d) (wordToWord64# 0xffffffffffffffff##) d
{-# INLINE recip_2by1# #-}

-- quotient and remainder of variable-length u divided by 64-bit d
quotrem_by1
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64 -- quotient
  -> PA.PrimArray Word64                      -- variable-length dividend
  -> Word64                                   -- divisor
  -> m Word64                                 -- remainder
quotrem_by1 q u d = do
  let !rec = recip_2by1 d
      loop !j !hj
        | j < 0 = pure hj
        | otherwise = do
            let !lj = PA.indexPrimArray u j
                !(P qj rj) = quotrem_2by1 hj lj d rec
            PA.writePrimArray q j qj
            loop (j - 1) rj
      !l  = PA.sizeofPrimArray u
      !hl = PA.indexPrimArray u (l - 1)
  loop (l - 2) hl

-- quotient and remainder of 256-bit u divided by 64-bit d
quotrem_4by1#
  :: (# Word64#, Word64#, Word64#, Word64# #) -- 256-bit dividend
  -> Word64#                                  -- 64-bit divisor
  -> (# Word64#, Word64#, Word64#, Word64# #) -- 192-bit quotient, 64-bit rem
quotrem_4by1# (# u0, u1, u2, u3 #) d =
  let !rec = recip_2by1# d
      !(# q2, r2 #) = quotrem_2by1# u3 u2 d rec
      !(# q1, r1 #) = quotrem_2by1# r2 u1 d rec
      !(# q0, r0 #) = quotrem_2by1# r1 u0 d rec
  in  (# q0, q1, q2, r0 #)
{-# INLINE quotrem_4by1# #-}

-- remainder of variable-length u divided by 64-bit d
rem_by1
  :: PA.PrimArray Word64                      -- variable-length dividend
  -> Word64                                   -- divisor
  -> Word64                                   -- remainder
rem_by1 u d = do
  let !rec = recip_2by1 d
      loop !j !hj
        | j < 0 = hj
        | otherwise = do
            let !lj = PA.indexPrimArray u j
                !(P _ rj) = quotrem_2by1 hj lj d rec
            loop (j - 1) rj
      !l  = PA.sizeofPrimArray u
      !hl = PA.indexPrimArray u (l - 1)
  loop (l - 2) hl

-- x =- y * m
-- requires (len x - x_offset) >= len y > 0
sub_mul
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> Int
  -> PA.PrimArray Word64
  -> Int
  -> Word64
  -> m Word64
sub_mul x x_offset y l (W64# m) = do
  let loop !j !borrow
        | j == l = pure (W64# borrow)
        | otherwise = do
            !(W64# x_j) <- PA.readPrimArray x (j + x_offset)
            let !(W64# y_j) = PA.indexPrimArray y j
            let !(# s, carry1 #) = sub_b# x_j borrow (wordToWord64# 0##)
                !(# ph, pl #)    = mul_c# y_j m
                !(# t, carry2 #) = sub_b# s pl (wordToWord64# 0##)
            PA.writePrimArray x (j + x_offset) (W64# t)
            loop (succ j) (plusWord64# (plusWord64# ph carry1) carry2)
  loop 0 (wordToWord64# 0##)

sub_mul#
  :: (# Word64#, Word64#, Word64#, Word64#, Word64# #) -- 320-bit dividend
  -> (# Word64#, Word64#, Word64#, Word64# #)          -- 256-bit divisor
  -> Word64#                                           -- 64-bit multiplier
  -> (# Word64#, Word64#, Word64#, Word64#, Word64# #) -- 256b diff, 64b rem
sub_mul# (# u0, u1, u2, u3, _ #) (# d0, d1, d2, d3 #) m =
  let !(# s_0,  c1_0 #) = sub_b# u0 (wordToWord64# 0##) (wordToWord64# 0##)
      !(# ph_0, pl_0 #) = mul_c# d0 m
      !(# t_0, c2_0 #)  = sub_b# s_0 pl_0 (wordToWord64# 0##)
      !b_0              = plusWord64# (plusWord64# ph_0 c1_0) c2_0

      !(# s_1,  c1_1 #) = sub_b# u1 b_0 (wordToWord64# 0##)
      !(# ph_1, pl_1 #) = mul_c# d1 m
      !(# t_1, c2_1 #)  = sub_b# s_1 pl_1 (wordToWord64# 0##)
      !b_1              = plusWord64# (plusWord64# ph_1 c1_1) c2_1

      !(# s_2,  c1_2 #) = sub_b# u2 b_1 (wordToWord64# 0##)
      !(# ph_2, pl_2 #) = mul_c# d2 m
      !(# t_2, c2_2 #)  = sub_b# s_2 pl_2 (wordToWord64# 0##)
      !b_2              = plusWord64# (plusWord64# ph_2 c1_2) c2_2

      !(# s_3,  c1_3 #) = sub_b# u3 b_2 (wordToWord64# 0##)
      !(# ph_3, pl_3 #) = mul_c# d3 m
      !(# t_3, c2_3 #)  = sub_b# s_3 pl_3 (wordToWord64# 0##)
      !b_3              = plusWord64# (plusWord64# ph_3 c1_3) c2_3
  in  (# t_0, t_1, t_2, t_3, b_3 #)
{-# INLINE sub_mul# #-}

-- x += y
add_to
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> Int
  -> PA.PrimArray Word64
  -> Int
  -> m Word64
add_to x x_offset y l = do
  let loop !j !cacc
        | j == l = pure cacc
        | otherwise = do
            xj <- PA.readPrimArray x (j + x_offset)
            let yj = PA.indexPrimArray y j
                !(P nex carry) = add_c xj yj cacc
            PA.writePrimArray x (j + x_offset) nex
            loop (succ j) carry
  loop 0 0

add_to#
  :: (# Word64#, Word64#, Word64#, Word64#, Word64# #) -- 320-bit dividend
  -> (# Word64#, Word64#, Word64#, Word64# #)          -- 256-bit divisor
  -> (# Word64#, Word64#, Word64#, Word64#, Word64# #) -- 256b sum, 64b carry
add_to# (# u0, u1, u2, u3, _ #) (# d0, d1, d2, d3 #) =
  let !(# t0, c0 #) = add_c# u0 d0 (wordToWord64# 0##)
      !(# t1, c1 #) = add_c# u1 d1 c0
      !(# t2, c2 #) = add_c# u2 d2 c1
      !(# t3, c3 #) = add_c# u3 d3 c2
  in  (# t0, t1, t2, t3, c3 #)
{-# INLINE add_to# #-}

-- knuth's algorithm 4.3.1 for variable-length division
-- divides normalized dividend by normalized divisor, writing quotient to
-- 'quo' and remainder to dividend
quotrem_knuth
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64  -- variable-length quotient
  -> PA.MutablePrimArray (PrimState m) Word64  -- normalized dividend
  -> Int                                       -- size of normalize dividend
  -> PA.PrimArray Word64                       -- normalized divisor
  -> m ()
quotrem_knuth quo u ulen d = do
  let !ld = PA.sizeofPrimArray d
      !dh = PA.indexPrimArray d (ld - 1)
      !dl = PA.indexPrimArray d (ld - 2)
      !rec = recip_2by1 dh
      loop !j
        | j < 0 = pure ()
        | otherwise = do
            !u2 <- PA.readPrimArray u (j + ld)
            !u1 <- PA.readPrimArray u (j + ld - 1)
            !u0 <- PA.readPrimArray u (j + ld - 2)
            let !qhat
                  | u2 >= dh  = 0xffff_ffff_ffff_ffff
                  | otherwise =
                      let !(P qh rh) = quotrem_2by1 u2 u1 dh rec
                          !(P ph pl) = mul_c qh dl
                      in  if   ph > rh || (ph == rh && pl > u0)
                          then qh - 1
                          else qh

            !borrow <- sub_mul u j d ld qhat
            PA.writePrimArray u (j + ld) (u2 - borrow)
            if   u2 < borrow
            then do
              -- rare case
              let !qh = qhat - 1
              r <- add_to u j d ld
              PA.writePrimArray u (j + ld) r
              PA.writePrimArray quo j qh
            else
              PA.writePrimArray quo j qhat
            loop (pred j)
  loop (ulen - ld - 1)

-- knuth's algorithm again, but only compute remainder
rem_knuth
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64  -- normalized dividend
  -> Int                                       -- size of normalize dividend
  -> PA.PrimArray Word64                       -- normalized divisor
  -> m ()
rem_knuth u ulen d = do
  let !ld = PA.sizeofPrimArray d
      !dh = PA.indexPrimArray d (ld - 1)
      !dl = PA.indexPrimArray d (ld - 2)
      !rec = recip_2by1 dh
      loop !j
        | j < 0 = pure ()
        | otherwise = do
            !u2 <- PA.readPrimArray u (j + ld)
            !u1 <- PA.readPrimArray u (j + ld - 1)
            !u0 <- PA.readPrimArray u (j + ld - 2)
            let !qhat
                  | u2 >= dh  = 0xffff_ffff_ffff_ffff
                  | otherwise =
                      let !(P qh rh) = quotrem_2by1 u2 u1 dh rec
                          !(P ph pl) = mul_c qh dl
                      in  if   ph > rh || (ph == rh && pl > u0)
                          then qh - 1
                          else qh

            !borrow <- sub_mul u j d ld qhat
            PA.writePrimArray u (j + ld) (u2 - borrow)
            if   u2 < borrow
            then do
              -- rare case
              r <- add_to u j d ld
              PA.writePrimArray u (j + ld) r
            else
              pure ()
            loop (pred j)
  loop (ulen - ld - 1)

normalized_dividend_length
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64 -- dividend
  -> m Int
normalized_dividend_length u = do
  !lu <- PA.getSizeofMutablePrimArray u
  let loop !j
        | j < 0 = pure 0
        | otherwise = do
            uj <- PA.readPrimArray u j
            if uj /= 0 then pure (j + 1) else loop (j - 1)
  loop (lu - 2) -- last word will be uninitialized, skip it
{-# INLINE normalized_dividend_length #-}

normalized_dividend_length_256#
  :: (# Word64#, Word64#, Word64#, Word64# #)
  -> Int#
normalized_dividend_length_256# (# u0, u1, u2, u3 #)
  | isTrue# (neWord64# u3 (wordToWord64# 0##)) = 4#
  | isTrue# (neWord64# u2 (wordToWord64# 0##)) = 3#
  | isTrue# (neWord64# u1 (wordToWord64# 0##)) = 2#
  | isTrue# (neWord64# u0 (wordToWord64# 0##)) = 1#
  | otherwise = 0#
{-# INLINE normalized_dividend_length_256# #-}

-- normalize 256-bit divisor
normalize_divisor
  :: PrimMonad m
  => Word256
  -> m (PA.PrimArray Word64, Int, Int, Word64) -- XX more efficient
normalize_divisor (Word256 d0 d1 d2 d3) = do
  let (dlen, d_last, shift)
        | d3 /= 0 = (4, d3, B.countLeadingZeros d3)
        | d2 /= 0 = (3, d2, B.countLeadingZeros d2)
        | d1 /= 0 = (2, d1, B.countLeadingZeros d1)
        | d0 /= 0 = (1, d0, B.countLeadingZeros d0)
        | otherwise = error "ppad-fixed (normalize): invalid 256-bit word"
  dn <- PA.newPrimArray dlen
  case dlen of
    4 -> do
      PA.writePrimArray dn 3 d3
      PA.writePrimArray dn 2 d2
      PA.writePrimArray dn 1 d1
      PA.writePrimArray dn 0 d0
    3 -> do
      PA.writePrimArray dn 2 d2
      PA.writePrimArray dn 1 d1
      PA.writePrimArray dn 0 d0
    2 -> do
      PA.writePrimArray dn 1 d1
      PA.writePrimArray dn 0 d0
    _ -> do
      PA.writePrimArray dn 0 d0
  let norm !j !dj
        | j == 0 = do
            let !dn_0 = dj .<<. shift
            PA.writePrimArray dn 0 dn_0
            pure dn_0
        | otherwise = do
            dj_1 <- PA.readPrimArray dn (j - 1)
            PA.writePrimArray dn j $
              (dj .<<. shift) .|. (dj_1 .>>. (64 - shift))
            norm (j - 1) dj_1
  dn_0 <- norm (dlen - 1) d_last
  d_final <- PA.unsafeFreezePrimArray dn
  pure (d_final, dlen, shift, dn_0)
{-# INLINE normalize_divisor #-}

-- normalize variable-length dividend
normalize_dividend
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> Int
  -> Int
  -> m ()
normalize_dividend u ulen s = do
  u_hi <- PA.readPrimArray u (ulen - 1)
  PA.writePrimArray u ulen (u_hi .>>. (64 - s))
  let loop !j !uj
        | j == 0 =
            PA.writePrimArray u 0 (uj .<<. s)
        | otherwise = do
            !uj_1 <- PA.readPrimArray u (j - 1)
            PA.writePrimArray u j $
              (uj .<<. s) .|. (uj_1 .>>. (64 - s))
            loop (j - 1) uj_1
  loop (ulen - 1) u_hi
{-# INLINE normalize_dividend #-}

-- quotient and remainder of variable-length u divided by variable-length d
quotrem
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64 -- quotient (potentially large)
  -> PA.MutablePrimArray (PrimState m) Word64 -- unnormalized dividend
  -> Word256                                  -- unnormalized divisor
  -> m (PA.PrimArray Word64)                  -- remainder (256-bit)
quotrem quo u d = do
  -- normalize divisor
  !(dn, dlen, shift, dn_0) <- normalize_divisor d
  -- get size of normalized dividend
  !ulen <- normalized_dividend_length u
  if   ulen < dlen
  then PA.freezePrimArray u 0 4
  else do
    -- normalize dividend
    normalize_dividend u ulen shift
    if   dlen == 1
    then do
     -- normalized divisor is small
      !un <- PA.unsafeFreezePrimArray u
      !r <- quotrem_by1 quo un dn_0
      pure $ PA.primArrayFromList [r .>>. shift, 0, 0, 0] -- XX
    else do
      -- quotrem of normalized dividend divided by normalized divisor
      quotrem_knuth quo u (ulen + 1) dn
      -- unnormalize remainder
      let unn_rem !j !unj
            | j == dlen = do
                PA.unsafeFreezePrimArray u
            | j + 1 == ulen = do
                PA.writePrimArray u j (unj .>>. shift)
                PA.unsafeFreezePrimArray u
            | otherwise = do
                !unj_1 <- PA.readPrimArray u (j + 1)
                PA.writePrimArray u j $
                  (unj .>>. shift) .|. (unj_1 .<<. (64 - shift))
                unn_rem (j + 1) unj_1

      !un_0 <- PA.readPrimArray u 0
      unn_rem 0 un_0
{-# INLINE quotrem #-}

-- quotient of u variable-length u divided by variable-length d
quot
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64 -- quotient
  -> PA.MutablePrimArray (PrimState m) Word64 -- unnormalized dividend
  -> Word256                                  -- unnormalized divisor
  -> m Int                                    -- length of quotient
quot quo u d = do
  -- normalize divisor
  !(dn, dlen, shift, dn_0) <- normalize_divisor d
  -- get size of normalized dividend
  !ulen <- normalized_dividend_length u
  if   ulen < dlen
  then pure 0
  else do
    normalize_dividend u ulen shift
    if   dlen == 1
    then do
     -- normalized divisor is small
      !un <- PA.unsafeFreezePrimArray u
      _ <- quotrem_by1 quo un dn_0
      pure ulen
    else do
      quotrem_knuth quo u (ulen + 1) dn
      pure (ulen + 1 - dlen)
{-# INLINE quot #-}

-- remainder of variable-length u divided by variable-length d
rem
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64 -- quotient  (potentially large)
  -> PA.MutablePrimArray (PrimState m) Word64 -- unnormalized dividend
  -> Word256                                  -- unnormalized divisor
  -> m (PA.PrimArray Word64)                  -- remainder (256-bit)
rem quo u d = do
  -- normalize divisor
  !(dn, dlen, shift, dn_0) <- normalize_divisor d
  -- get size of normalized dividend
  !ulen <- normalized_dividend_length u
  if   ulen < dlen
  then PA.freezePrimArray u 0 4
  else do
    -- normalize dividend
    normalize_dividend u ulen shift
    if   dlen == 1
    then do
     -- normalized divisor is small
      !un <- PA.unsafeFreezePrimArray u
      !r <- quotrem_by1 quo un dn_0
      pure $ PA.primArrayFromList [r .>>. shift, 0, 0, 0] -- XX
    else do
      -- quotrem of normalized dividend divided by normalized divisor
      rem_knuth u (ulen + 1) dn
      -- unnormalize remainder
      let unn_rem !j !unj
            | j == dlen = do
                PA.unsafeFreezePrimArray u
            | j + 1 == ulen = do
                PA.writePrimArray u j (unj .>>. shift)
                PA.unsafeFreezePrimArray u
            | otherwise = do
                !unj_1 <- PA.readPrimArray u (j + 1)
                PA.writePrimArray u j $
                  (unj .>>. shift) .|. (unj_1 .<<. (64 - shift))
                unn_rem (j + 1) unj_1

      !un_0 <- PA.readPrimArray u 0
      unn_rem 0 un_0
{-# INLINE rem #-}

-- | Division on 'Word256' values.
--
--   >>> to_word256 0xFFFFFFFFFF `div` to_word256 0xFFFFFF
--   65536
div :: Word256 -> Word256 -> Word256
div u@(Word256 u0 u1 u2 u3) d@(Word256 d0 _ _ _)
  | is_zero d || d `gt` u = zero -- ?
  | u == d                = one
  | is_word64 u           = Word256 (u0 `Prelude.quot` d0) 0 0 0
  | otherwise = runST $ do
      -- allocate quotient
      quo <- PA.newPrimArray 4
      -- allocate dividend, leaving enough space for normalization
      u_arr <- PA.newPrimArray 5
      PA.writePrimArray u_arr 0 u0
      PA.writePrimArray u_arr 1 u1
      PA.writePrimArray u_arr 2 u2
      PA.writePrimArray u_arr 3 u3
      -- last index of u_hot intentionally unset
      l <- quot quo u_arr d
      case l of
        1 -> do
          q0 <- PA.readPrimArray quo 0
          pure (Word256 q0 0 0 0)
        2 -> do
          q0 <- PA.readPrimArray quo 0
          q1 <- PA.readPrimArray quo 1
          pure (Word256 q0 q1 0 0)
        3 -> do
          q0 <- PA.readPrimArray quo 0
          q1 <- PA.readPrimArray quo 1
          q2 <- PA.readPrimArray quo 2
          pure (Word256 q0 q1 q2 0)
        4 -> do
          q0 <- PA.readPrimArray quo 0
          q1 <- PA.readPrimArray quo 1
          q2 <- PA.readPrimArray quo 2
          q3 <- PA.readPrimArray quo 3
          pure (Word256 q0 q1 q2 q3)
        _ -> error "ppad-fixed (quot): invalid quotient length"

-- | Modulo operation on 'Word256' values.
--
--   >>> to_word256 0xFFFFFFFFFF `mod` to_word256 0xFFFFFF
--   65535
mod :: Word256 -> Word256 -> Word256
mod u@(Word256 u0 u1 u2 u3) d@(Word256 d0 _ _ _)
  | is_zero d || d `gt` u = zero -- ?
  | u == d                = one
  | is_word64 u           = Word256 (u0 `Prelude.rem` d0) 0 0 0
  | otherwise = runST $ do
      -- allocate quotient
      quo <- PA.newPrimArray 4
      -- allocate dividend, leaving enough space for normalization
      u_hot <- PA.newPrimArray 5
      PA.writePrimArray u_hot 0 u0
      PA.writePrimArray u_hot 1 u1
      PA.writePrimArray u_hot 2 u2
      PA.writePrimArray u_hot 3 u3
      -- last index of u_hot intentionally unset
      r <- rem quo u_hot d
      let r0 = PA.indexPrimArray r 0
          r1 = PA.indexPrimArray r 1
          r2 = PA.indexPrimArray r 2
          r3 = PA.indexPrimArray r 3
      pure (Word256 r0 r1 r2 r3)
