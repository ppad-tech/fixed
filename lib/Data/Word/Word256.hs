{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.Word.Word256
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Large fixed-width words, complete with support for conversion,
-- comparison, bitwise operations, arithmetic, and modular arithmetic.

-- module Data.Word.Word256 (
--     Word256(..)
--   , zero
--   , one
--
--   -- * Conversion
--   , to_integer
--   , to_word256
--
--   -- * Comparison
--   , lt
--   , gt
--   , is_zero
--
--   -- * Bit Operations
--   , or
--   , and
--   , xor
--
--   -- * Arithmetic
--   , add
--   , sub
--   , mul
--
--   -- for testing/benchmarking
--   , mul_c
--   , mul_c#
--   , umul_hop#
--   , umul_step#
--   , mul_512#
--   ) where

module Data.Word.Word256 where

import Control.DeepSeq
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.), (.^.))
import GHC.Exts
import GHC.Generics
import GHC.Word
import Prelude hiding (div, mod, or, and, quot, rem)

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
  deriving (Eq, Show, Generic)

instance NFData Word256

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
-- like crypto-bigint's widening_mul, except that's (lo, hi)
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

select# :: Word32# -> Word32# -> Int# -> Word32#
select# a b c = xorWord32#
  a
  (andWord32# (int32ToWord32# (intToInt32# c)) (xorWord32# a b))
{-# INLINE select# #-}

short_div# :: Word32# -> Word32# -> Word32# -> Word32# -> Word32#
short_div# dividend dividend_bits divisor divisor_bits =
    let !dif = int32ToInt#
          (word32ToInt32# (subWord32# dividend_bits divisor_bits))
        !divisor0 = uncheckedShiftLWord32# divisor dif
        !j0       = dif +# 1#
    in  loop j0 (wordToWord32# 0##) dividend divisor0
  where
    loop !j !quo !div !dis
      | isTrue# (j ># 0#) =
          let !nj   = j -# 1#
              !bit  = ltWord32# div dis
              !ndiv = select# (subWord32# div dis) div bit
              !ndis = uncheckedShiftRLWord32# dis 1#
              !nquo = orWord32# quo
                (uncheckedShiftLWord32#
                  (uncheckedShiftRLWord32# (notWord32# quo) 31#)
                  nj)
          in  loop nj nquo ndiv ndis
      | otherwise =
          quo
{-# INLINE short_div# #-}

short_div :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
short_div (W32# a) (W32# b) (W32# c) (W32# d) = W32# (short_div# a b c d)
