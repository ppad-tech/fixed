{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Word.Wide where

import Control.DeepSeq
import Data.Choice
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import qualified Data.Bits as B
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- add-with-carry, (# sum, carry bit #)
add_c# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
add_c# a b c =
  let !(# c0, s0 #) = plusWord2# a b
      !(# c1,  s #) = plusWord2# s0 c
      !o = or# c0 c1
  in  (# s, o #)
{-# INLINE add_c# #-}

-- subtract-with-borrow, (# difference, borrow bit #)
sub_b# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
sub_b# m n b =
  let !(# d0, b0 #) = subWordC# m n
      !(#  d, b1 #) = subWordC# d0 b
      !c = int2Word# (orI# b0 b1)
  in  (# d, c #)
{-# INLINE sub_b# #-}

-- (# lo, hi #)
mul_c# :: Word# -> Word# -> (# Word#, Word# #)
mul_c# a b =
  let !(# h, l #) = timesWord2# a b
  in  (# l, h #)
{-# INLINE mul_c# #-}

mul_c :: Word -> Word -> Wide
mul_c (W# a) (W# b) = Wide (mul_c# a b)

-- constant-time quotient, given maximum bitsizes
div1by1# :: Word# -> Word# -> Word# -> Word# -> Word#
div1by1# dividend dividend_bits divisor divisor_bits =
    let !dif      = word2Int# (minusWord# dividend_bits divisor_bits)
        !divisor0 = uncheckedShiftL# divisor dif
        !j0       = dif +# 1#
    in  loop j0 0## dividend divisor0
  where
    !size# = case B.finiteBitSize (0 :: Word) of I# n# -> n#
    loop !j !quo !div !dis
      | isTrue# (j ># 0#) =
          let !nj   = j -# 1#
              -- the following CT logic doesn't use Data.Choice because
              -- of inlining rules (GHC won't inline in a recursive
              -- binding like 'loop')
              !bit  = negateInt# (ltWord# div dis) -- ct
              !ndiv = let a = minusWord# div dis   -- ct
                      in  xor# a (and# (int2Word# bit) (xor# a div))
              !ndis = uncheckedShiftRL# dis 1#
              !nquo = or# quo
                (uncheckedShiftL#
                  (uncheckedShiftRL# (not# quo) (size# -# 1#))
                  nj)
          in  loop nj nquo ndiv ndis
      | otherwise =
          quo
{-# INLINE div1by1# #-}

-- short (one-word) division
div1by1 :: Word -> Word -> Word -> Word -> Word
div1by1 (W# a) (W# b) (W# c) (W# d) = W# (div1by1# a b c d)

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

-- addition, subtraction ------------------------------------------------------

-- wide-add-with-carry, i.e. (# sum, carry bit #)
add_wc#
  :: (# Word#, Word# #)
  -> (# Word#, Word# #)
  -> (# Word#, Word#, Word# #)
add_wc# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = add_c# a0 b0 0##
      !(# s1, c1 #) = add_c# a1 b1 c0
  in  (# s0, s1, c1 #)
{-# INLINE add_wc# #-}

-- wide addition (wrapping)
add_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
add_w# a b =
  let !(# c0, c1, _ #) = add_wc# a b
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
  let !(# s0, c0 #) = sub_b# a0 b0 0##
      !(# s1, c1 #) = sub_b# a1 b1 c0
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
  let !(# p0_lo, p0_hi #) = mul_c# a0 b0
      !(# p1_lo, _ #) = mul_c# a0 b1
      !(# p2_lo, _ #) = mul_c# a1 b0
      !(# s0, _ #) = add_c# p0_hi p1_lo 0##
      !(# s1, _ #) = add_c# s0 p2_lo 0##
  in  (# p0_lo, s1 #)
{-# INLINE mul_w# #-}

mul_w :: Wide -> Wide -> Wide
mul_w (Wide a) (Wide b) = Wide (mul_w# a b)

-- division -------------------------------------------------------------------

-- normalized divisor, shift, reciprocal
newtype Reciprocal = Reciprocal (# Word#, Int#, Word# #)

-- XX different versions should be defined depending on word size, via CPP,
--    but for now this is implicitly hard-coded to 64-bit
--
-- reciprocal of a divisor, given its highest bit is set
recip# :: Word# -> Word#
recip# d =
  let !d0  = and# d 1##
      !d9  = uncheckedShiftRL# d 55#
      !d40 = plusWord# (uncheckedShiftRL# d 24#) 1##
      !d63 = plusWord# (uncheckedShiftRL# d 1#) d0
      !v0  = div1by1# 0x07fd00## 19## d9 9## -- (1 << 19) - 3 * (1 << 8)
      !v1 =
        minusWord#
          (minusWord#
            (uncheckedShiftL# v0 11#)
            (uncheckedShiftRL#
              (timesWord# v0 (timesWord# v0 d40))
              40#))
          1##
      !v2 =
        plusWord#
          (uncheckedShiftL# v1 13#)
          (uncheckedShiftRL#
            (timesWord# v1
              (minusWord#
                0x10000000_00000000## -- 1 << 60
                (timesWord# v1 d40)))
            47#)
      !e =
        plusWord#
          (plusWord#
            (minusWord#
              0xffffffff_ffffffff##
              (timesWord# v2 d63))
            1##)
          (timesWord# (uncheckedShiftRL# v2 1#) d0)
      !(# _, hi_0 #) = mul_c# v2 e
      !v3 =
        plusWord#
          (uncheckedShiftL# v2 31#)
          (uncheckedShiftRL# hi_0 1#)
      !x = plusWord# v3 1##
      !(# _, hi_1 #) = mul_c# x d
      !hi_2 = ct_select_word# d hi_1 (from_word_nonzero# x)
  in  minusWord# (minusWord# v3 hi_2) d
{-# INLINE recip# #-}

-- reciprocal of a divisor, given highest bit is set
recip :: Word -> Word
recip (W# divisor) = W# (recip# divisor)

new_recip# :: Word# -> Reciprocal
new_recip# w =
  let !s = word2Int# (clz# w)
      !d = uncheckedShiftL# w s
  in  Reciprocal (# d, s, recip# d #)
{-# INLINE new_recip# #-}

new_recip :: Word -> Reciprocal
new_recip (W# w) = new_recip# w

-- quotient and remainder of wide word (lo, hi), divided by divisor
quotrem2by1# :: (# Word#, Word# #) -> Word# -> (# Word#, Word# #)
quotrem2by1# (# l, h #) d = quotRemWord2# h l d
{-# INLINE quotrem2by1# #-}

-- ~6x slower than div2by1, but useful for testing
quotrem2by1 :: Wide -> Word -> (Word, Word)
quotrem2by1 (Wide u) (W# d) =
  let !(# q, r #) = quotrem2by1# u d
  in  (W# q, W# r)

-- quotient and remainder of wide word (lo, hi) divided using reciprocal
div2by1# :: (# Word#, Word# #) -> Reciprocal -> (# Word#, Word# #)
div2by1# (# u0, u1 #) (Reciprocal (# d, _, r #)) =
  let !(# q0_0, q1_0 #) = mul_c# r u1
      !(# q0_1, q1_1 #) = add_w# (# q0_0, q1_0 #) (# u0, u1 #)
      !q1_2 = plusWord# q1_1 1##
      !r_0  = minusWord# u0 (timesWord# q1_2 d)
      -- ct block 1
      !r_gt_q0 = from_word_lt# q0_1 r_0
      !q1_3 = ct_select_word# q1_2 (minusWord# q1_2 1##) r_gt_q0
      !r_1  = ct_select_word# r_0 (plusWord# r_0 d) r_gt_q0
      -- ct block 2
      !r_ge_d = from_word_le# d r_1
      !q1_4 = ct_select_word# q1_3 (plusWord# q1_3 1##) r_ge_d
      !r_2  = ct_select_word# r_1 (minusWord# r_1 d) r_ge_d
  in  (# q1_4, r_2 #)
{-# INLINE div2by1# #-}

-- quotient and remainder of wide word divided by word
div2by1 :: Wide -> Word -> (Word, Word)
div2by1 (Wide (# u0, u1 #)) d =
  let !re = new_recip d
      !(# q, r #) = div2by1# (# u0, u1 #) re
  in  (W# q, W# r)

