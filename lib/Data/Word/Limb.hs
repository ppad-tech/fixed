{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Word.Limb (
  -- * Unboxed Arithmetic
    add_c#
  , sub_b#
  , mul_c#
  , mul_w#
  , mac#
  ) where

import GHC.Exts

-- addition, subtraction ------------------------------------------------------

-- | Overflowing addition, computing 'a + b + c'.
--
--   The new carry will either be 0, 1, or 2.
add_c#
  :: Word#              -- ^ augend
  -> Word#              -- ^ addend
  -> Word#              -- ^ carry
  -> (# Word#, Word# #) -- ^ (# sum, new carry #)
add_c# a b c =
  let !(# c0, s0 #) = plusWord2# a b
      !(# c1,  s #) = plusWord2# s0 c
  in  (# s, or# c0 c1 #)
{-# INLINE add_c# #-}

-- | Borrowing subtraction, computing 'm - (n + b)'.
sub_b#
  :: Word#              -- ^ minuend
  -> Word#              -- ^ subtrahend
  -> Word#              -- ^ borrow
  -> (# Word#, Word# #) -- ^ (# difference, new borrow #)
sub_b# m n b =
  let !(# d0, b0 #) = subWordC# m n
      !(#  d, b1 #) = subWordC# d0 b
      !c = int2Word# (orI# b0 b1)
  in  (# d, c #)
{-# INLINE sub_b# #-}

-- multiplication -------------------------------------------------------------

-- | Widening multiplication, returning low and high words of the result.
mul_c#
  :: Word#              -- ^ multiplicand
  -> Word#              -- ^ multiplier
  -> (# Word#, Word# #) -- ^ (# low, high #) product
mul_c# a b =
  let !(# h, l #) = timesWord2# a b
  in  (# l, h #)
{-# INLINE mul_c# #-}

-- | Wrapping multiplication, returning only the low word of the result.
mul_w#
  :: Word# -- ^ multiplicand
  -> Word# -- ^ multiplier
  -> Word# -- ^ low-word of product
mul_w# a b =
  let !(# _, l #) = timesWord2# a b
  in  l
{-# INLINE mul_w# #-}

-- | Multiply-add-carry, computing 'a * b + m + c'.
mac#
  :: Word#              -- ^ multiplicand
  -> Word#              -- ^ multiplier
  -> Word#              -- ^ addend
  -> Word#              -- ^ carry
  -> (# Word#, Word# #) -- ^ lhs * rhs + addend + carry
mac# a b m c =
    let !(# l_0, h_0 #) = wadd_w# (mul_c# a b) m
        !(# d, l_1 #) = plusWord2# l_0 c
        !h_1 = plusWord# h_0 d
    in  (# l_1, h_1 #)
  where
    -- wide wrapping addition
    wadd_w# :: (# Word#, Word# #) -> Word# -> (# Word#, Word# #)
    wadd_w# (# x_lo, x_hi #) y_lo =
      let !(# c0, s0 #) = plusWord2# x_lo y_lo
          !(# _, s1 #) = plusWord2# x_hi c0
      in  (# s0, s1 #)
    {-# INLINE wadd_w# #-}
{-# INLINE mac# #-}

