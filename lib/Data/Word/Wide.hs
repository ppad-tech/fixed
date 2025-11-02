{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Word.Wide (
  -- * Wide Words
    Wide(..)
  , get_lo
  , get_hi

  -- * Construction, Conversion
  , lo
  , hi
  , wide
  , to
  , from

  , lo#
  , hi#

  -- * Bit Manipulation
  , or
  , and
  , xor
  , not
  , shr
  , unchecked_shr

  -- * Arithmetic
  , add
  , sub
  , mul
  , quotrem_by1
  , _quotrem_by1

  , add_w#
  , mul_w#
  ) where

import Control.DeepSeq
import qualified Data.Choice as C
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import qualified Data.Bits as B
import qualified Data.Word.Limb as L
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- wide words -----------------------------------------------------------------

-- little-endian, i.e. (# lo, hi #)
data Wide = Wide (# Word#, Word# #)

get_lo# :: (# Word#, Word# #) -> Word#
get_lo# (# l, _ #) = l
{-# INLINE get_lo# #-}

get_lo :: Wide -> Word
get_lo (Wide w) = W# (get_lo# w)

get_hi# :: (# Word#, Word# #) -> Word#
get_hi# (# _, h #) = h
{-# INLINE get_hi# #-}

get_hi :: Wide -> Word
get_hi (Wide w) = W# (get_hi# w)

instance Show Wide where
  show (Wide (# a, b #)) = "(" <> show (W# a) <> ", " <> show (W# b) <> ")"

instance Eq Wide where
  Wide (# a, b #) == Wide (# c, d #) =
    isTrue# (andI# (eqWord# a c) (eqWord# b d))

instance NFData Wide where
  rnf (Wide a) = case a of (# _, _ #) -> ()

-- construction / conversion --------------------------------------------------

-- construct from hi
hi# :: Word# -> (# Word#, Word# #)
hi# w = (# 0##, w #)
{-# INLINE hi# #-}

hi :: Word -> Wide
hi (W# w) = Wide (hi# w)

-- construct from lo
lo# :: Word# -> (# Word#, Word# #)
lo# w = (# w, 0## #)
{-# INLINE lo# #-}

lo :: Word -> Wide
lo (W# w) = Wide (lo# w)

-- construct from lo, hi
wide :: Word -> Word -> Wide
wide (W# l) (W# h) = Wide (# l, h #)

to :: Integer -> Wide
to n =
  let !size = B.finiteBitSize (0 :: Word)
      !mask = fi (maxBound :: Word) :: Integer
      !(W# w0) = fi (n .&. mask)
      !(W# w1) = fi ((n .>>. size) .&. mask)
  in  Wide (# w0, w1 #)

from :: Wide -> Integer
from (Wide (# a, b #)) =
      fi (W# b) .<<. (B.finiteBitSize (0 :: Word))
  .|. fi (W# a)

-- bits -----------------------------------------------------------------------

or_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
or_w# (# a0, a1 #) (# b0, b1 #) = (# or# a0 b0, or# a1 b1 #)
{-# INLINE or_w# #-}

or :: Wide -> Wide -> Wide
or (Wide a) (Wide b) = Wide (or_w# a b)

and_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
and_w# (# a0, a1 #) (# b0, b1 #) = (# and# a0 b0, and# a1 b1 #)
{-# INLINE and_w# #-}

and :: Wide -> Wide -> Wide
and (Wide a) (Wide b) = Wide (and_w# a b)

xor_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
xor_w# (# a0, a1 #) (# b0, b1 #) = (# xor# a0 b0, xor# a1 b1 #)
{-# INLINE xor_w# #-}

xor :: Wide -> Wide -> Wide
xor (Wide a) (Wide b) = Wide (xor_w# a b)

not_w# :: (# Word#, Word# #) -> (# Word#, Word# #)
not_w# (# a0, a1 #) = (# not# a0, not# a1 #)
{-# INLINE not_w# #-}

not :: Wide -> Wide
not (Wide w) = Wide (not_w# w)
{-# INLINE not #-}

-- overflowing, vartime w/respect to s
shr_of_vartime# :: (# Word#, Word# #) -> Int# -> C.MaybeWide#
shr_of_vartime# (# l, h #) s
    | isTrue# (s ># wide_size) = C.none_wide# (# 0##, 0## #)
    | otherwise =
        let !(# shift_num, rem #) = quotRemInt# s size
            !w_1 = case shift_num of
              0# ->
                let !h_0 = uncheckedShiftRL# h rem
                    !car = uncheckedShiftL# h (size -# rem)
                    !shf = uncheckedShiftRL# l rem
                    !l_0 = or# shf car
                in  (# l_0, h_0 #)
              1# ->
                let !l_0 = uncheckedShiftRL# h rem
                in  (# l_0, 0## #)
              2# ->
                (# l, h #)
              _  -> error "ppad-fixed (shr_of_vartime#): internal error"
        in  C.some_wide# w_1
  where
    !size = case B.finiteBitSize (0 :: Word) of I# m -> m
    !wide_size = 2# *# size
{-# INLINE shr_of_vartime# #-}

shr_of# :: (# Word#, Word# #) -> Int# -> C.MaybeWide#
shr_of# (# l, h #) s =
    let !shift_bits = size -# (word2Int# (clz# (int2Word# (wide_size -# 1#))))
        !overflow = C.not_c#
          (C.from_word_lt# (int2Word# s) (int2Word# wide_size))
        !shift = remWord# (int2Word# s) (int2Word# wide_size)
        loop !j !res
          | isTrue# (j <# shift_bits) =
              let !bit = C.from_word_lsb# -- XX not inlined
                    (and# (uncheckedShiftRL# shift j) 1##)
                  !nres = C.ct_select_wide# -- XX
                    res
                    (C.expect_wide# -- XX
                      (shr_of_vartime# -- XX
                        res
                        (word2Int# (uncheckedShiftL# 1## j)))
                      "shift within range")
                    bit
              in  loop (j +# 1#) nres
          | otherwise = res
        !result = loop 0# (# l, h #)
    in  C.just_wide#
          (C.ct_select_wide# result (# 0##, 0## #) overflow)
          (C.not_c# overflow)
  where
    !size = case B.finiteBitSize (0 :: Word) of I# m -> m
    !wide_size = 2# *# size
{-# INLINE shr_of# #-}

shr# :: (# Word#, Word# #) -> Int# -> (# Word#, Word# #)
shr# w s = C.expect_wide# (shr_of# w s) "invalid shift"
{-# INLINE shr# #-}

-- wrapping
unchecked_shr# :: (# Word#, Word# #) -> Int# -> (# Word#, Word# #)
unchecked_shr# w s = C.expect_wide_or# (shr_of# w s) (# 0##, 0## #)
{-# INLINE unchecked_shr# #-}

-- constant-time shr, ErrorCall on invalid shift
shr :: Wide -> Int -> Wide
shr (Wide w) (I# s) = Wide (shr# w s)

-- constant-time shr, saturating
unchecked_shr :: Wide -> Int -> Wide
unchecked_shr (Wide w) (I# s) = Wide (unchecked_shr# w s)

-- addition, subtraction ------------------------------------------------------

-- wide addition (overflowing)
add_c#
  :: (# Word#, Word# #)
  -> (# Word#, Word# #)
  -> (# Word#, Word#, Word# #) -- (# sum, carry bit #)
add_c# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.add_c# a0 b0 0##
      !(# s1, c1 #) = L.add_c# a1 b1 c0
  in  (# s0, s1, c1 #)
{-# INLINE add_c# #-}

-- wide addition (wrapping)
add_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
add_w# a b =
  let !(# c0, c1, _ #) = add_c# a b
  in  (# c0, c1 #)
{-# INLINE add_w# #-}

-- wide addition (wrapping)
add :: Wide -> Wide -> Wide
add (Wide a) (Wide b) = Wide (add_w# a b)

-- wide subtract-with-borrow
sub_wb#
  :: (# Word#, Word# #)
  -> (# Word#, Word# #)
  -> (# Word#, Word#, Word# #)
sub_wb# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.sub_b# a0 b0 0##
      !(# s1, c1 #) = L.sub_b# a1 b1 c0
  in  (# s0, s1, c1 #)
{-# INLINE sub_wb# #-}

-- wide subtraction (wrapping)
sub_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
sub_w# a b =
  let !(# c0, c1, _ #) = sub_wb# a b
  in  (# c0, c1 #)
{-# INLINE sub_w# #-}

-- wide subtraction (wrapping)
sub :: Wide -> Wide -> Wide
sub (Wide a) (Wide b) = Wide (sub_w# a b)

-- multiplication -------------------------------------------------------------

-- wide multiplication (wrapping)
mul_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
mul_w# (# a0, a1 #) (# b0, b1 #) =
  let !(# p0_lo, p0_hi #) = L.mul_c# a0 b0
      !(# p1_lo, _ #) = L.mul_c# a0 b1
      !(# p2_lo, _ #) = L.mul_c# a1 b0
      !(# s0, _ #) = L.add_c# p0_hi p1_lo 0##
      !(# s1, _ #) = L.add_c# s0 p2_lo 0##
  in  (# p0_lo, s1 #)
{-# INLINE mul_w# #-}

mul :: Wide -> Wide -> Wide
mul (Wide a) (Wide b) = Wide (mul_w# a b)

-- division -------------------------------------------------------------------

-- quotient and remainder of wide word (lo, hi), divided by divisor
_quotrem_by1# :: (# Word#, Word# #) -> Word# -> (# Word#, Word# #)
_quotrem_by1# (# l, h #) d = quotRemWord2# h l d
{-# INLINE _quotrem_by1# #-}

-- ~6x slower than quotrem_by1, but useful for testing
_quotrem_by1 :: Wide -> Word -> (Word, Word)
_quotrem_by1 (Wide u) (W# d) =
  let !(# q, r #) = _quotrem_by1# u d
  in  (W# q, W# r)

-- quotient and remainder of wide word (lo, hi) divided using reciprocal
quotrem_by1# :: (# Word#, Word# #) -> L.Reciprocal -> (# Word#, Word# #)
quotrem_by1# (# u0, u1 #) (L.Reciprocal (# d, _, r #)) =
  let !(# q0_0, q1_0 #) = L.mul_c# r u1
      !(# q0_1, q1_1 #) = add_w# (# q0_0, q1_0 #) (# u0, u1 #)
      !q1_2 = plusWord# q1_1 1##
      !r_0  = minusWord# u0 (timesWord# q1_2 d)
      -- ct block 1
      !r_gt_q0 = C.from_word_lt# q0_1 r_0
      !q1_3 = C.ct_select_word# q1_2 (minusWord# q1_2 1##) r_gt_q0
      !r_1  = C.ct_select_word# r_0 (plusWord# r_0 d) r_gt_q0
      -- ct block 2
      !r_ge_d = C.from_word_le# d r_1
      !q1_4 = C.ct_select_word# q1_3 (plusWord# q1_3 1##) r_ge_d
      !r_2  = C.ct_select_word# r_1 (minusWord# r_1 d) r_ge_d
  in  (# q1_4, r_2 #)
{-# INLINE quotrem_by1# #-}

-- quotient and remainder of wide word divided by word
quotrem_by1 :: Wide -> Word -> (Word, Word)
quotrem_by1 (Wide (# u0, u1 #)) (W# d) =
  let !re = L.recip# d
      !(# q, r #) = quotrem_by1# (# u0, u1 #) re
  in  (W# q, W# r)

