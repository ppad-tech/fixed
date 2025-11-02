{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Word.Limb (
  -- * Unlifted Arithmetic
    add_c#
  , sub_b#
  , mul_c#
  , mul_w#
  , recip#
  , quot#
  , mul_add_c#

  -- * Reciprocal
  , Reciprocal(..)

  -- * Boxed Wrappers
  , quot
  , recip
  ) where

import Data.Choice
import qualified Data.Bits as B
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- addition, subtraction ------------------------------------------------------

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

-- multiplication -------------------------------------------------------------

-- (# lo, hi #)
mul_c# :: Word# -> Word# -> (# Word#, Word# #)
mul_c# a b =
  let !(# h, l #) = timesWord2# a b
  in  (# l, h #)
{-# INLINE mul_c# #-}

-- wrapping multiplication
mul_w# :: Word# -> Word# -> Word#
mul_w# a b =
  let !(# _, l #) = timesWord2# a b
  in  l
{-# INLINE mul_w# #-}

-- carrying multiplication with addition
mul_add_c#
  :: Word# -- lhs
  -> Word# -- rhs
  -> Word# -- addend
  -> Word# -- carry
  -> (# Word#, Word# #)  -- lhs * rhs + addend + carry
mul_add_c# lhs rhs addend carry =
    let !(# l_0, h_0 #) = add_w# (mul_c# lhs rhs) (# addend, 0## #)
        !(# l_1, c #) = add_c# l_0 carry 0##
        !h_1 = plusWord# h_0 c
    in  (# l_1, h_1 #)
  where
    -- duplicated w/Data.Word.Wide to avoid awkward module structuring
    -- wide addition with carry
    add_wc#
      :: (# Word#, Word# #)
      -> (# Word#, Word# #)
      -> (# Word#, Word#, Word# #)
    add_wc# (# a0, a1 #) (# b0, b1 #) =
      let !(# s0, c0 #) = add_c# a0 b0 0##
          !(# s1, c1 #) = add_c# a1 b1 c0
      in  (# s0, s1, c1 #)
    {-# INLINE add_wc# #-}

    -- wide wrapping addition
    add_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
    add_w# a b =
      let !(# c0, c1, _ #) = add_wc# a b
      in  (# c0, c1 #)
    {-# INLINE add_w# #-}
{-# INLINE mul_add_c# #-}

-- division -------------------------------------------------------------------

-- normalized divisor, shift, reciprocal
newtype Reciprocal = Reciprocal (# Word#, Int#, Word# #)

-- XX different versions should be defined depending on word size, via CPP,
--    but for now this is implicitly hard-coded to 64-bit
--
-- reciprocal of a divisor, given its highest bit is set
_recip# :: Word# -> Word#
_recip# d =
  let !d0  = and# d 1##
      !d9  = uncheckedShiftRL# d 55#
      !d40 = plusWord# (uncheckedShiftRL# d 24#) 1##
      !d63 = plusWord# (uncheckedShiftRL# d 1#) d0
      !v0  = quot# 0x07fd00## 19## d9 9## -- (1 << 19) - 3 * (1 << 8)
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
{-# INLINE _recip# #-}

recip# :: Word# -> Reciprocal
recip# w =
  let !s = word2Int# (clz# w)
      !d = uncheckedShiftL# w s
  in  Reciprocal (# d, s, _recip# d #)
{-# INLINE recip# #-}

-- constant-time quotient, given maximum bitsizes
quot# :: Word# -> Word# -> Word# -> Word# -> Word#
quot# dividend dividend_bits divisor divisor_bits =
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
{-# INLINE quot# #-}

-- boxed ----------------------------------------------------------------------

-- short (one-word) division
quot :: Word -> Word -> Word -> Word -> Word
quot (W# a) (W# b) (W# c) (W# d) = W# (quot# a b c d)

recip :: Word -> Word
recip (W# w) = case recip# w of
  Reciprocal (# _, _, r #) -> W# r

