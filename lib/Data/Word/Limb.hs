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

import qualified Data.Bits as B
import GHC.Exts (Word#)
import qualified GHC.Exts as Exts

newtype Limb = Limb Word#

render :: Limb -> String
render (Limb a) = show (Exts.W# a)

-- bit manipulation -----------------------------------------------------------

and# :: Limb -> Limb -> Limb
and# (Limb a) (Limb b) = Limb (Exts.and# a b)
{-# INLINE and# #-}

or# :: Limb -> Limb -> Limb
or# (Limb a) (Limb b) = Limb (Exts.or# a b)
{-# INLINE or# #-}

not# :: Limb -> Limb
not# (Limb a) = Limb (Exts.not# a)
{-# INLINE not# #-}

xor# :: Limb -> Limb -> Limb
xor# (Limb a) (Limb b) = Limb (Exts.xor# a b)
{-# INLINE xor# #-}

bits :: Limb -> Int
bits (Limb a) =
  let !_BITS = B.finiteBitSize (0 :: Word)
      !zs = B.countLeadingZeros (Exts.W# a)
  in  _BITS - zs
{-# INLINE bits #-}

-- addition -------------------------------------------------------------------

-- | Overflowing addition, computing augend + addend, returning the
--   sum and carry.
add_o#
  :: Limb             -- ^ augend
  -> Limb             -- ^ addend
  -> (# Limb, Limb #) -- ^ (# sum, carry #)
add_o# (Limb a) (Limb b) = case Exts.plusWord2# a b of
  (# c, s #) -> (# Limb s, Limb c #)
{-# INLINE add_o# #-}

-- | Carrying addition, computing augend + addend + carry, returning
--   the sum and new carry.
add_c#
  :: Limb             -- ^ augend
  -> Limb             -- ^ addend
  -> Limb             -- ^ carry
  -> (# Limb, Limb #) -- ^ (# sum, new carry #)
add_c# (Limb a) (Limb b) (Limb c) =
  let !(# c0, s0 #) = Exts.plusWord2# a b
      !(# c1,  s #) = Exts.plusWord2# s0 c
  in  (# Limb s, Limb (Exts.or# c0 c1) #)
{-# INLINE add_c# #-}

-- | Wrapping addition, computing augend + addend, returning the sum
--   (discarding overflow).
add_w#
  :: Limb -- ^ augend
  -> Limb -- ^ addend
  -> Limb -- ^ sum
add_w# (Limb a) (Limb b) = Limb (Exts.plusWord# a b)
{-# INLINE add_w# #-}

-- | Saturating addition, computing augend + addend, returning the
--   sum (clamping to the maximum representable value in the case of
--   overflow).
add_s#
  :: Limb
  -> Limb
  -> Limb
add_s# (Limb a) (Limb b) = case Exts.addWordC# a b of
  (# s, 0# #) -> Limb s
  _ -> case maxBound :: Word of
    Exts.W# m -> Limb m
{-# INLINE add_s# #-}

-- subtraction ----------------------------------------------------------------

-- | Borrowing subtraction, computing minuend - (subtrahend + borrow),
--   returning the difference and new borrow mask.
sub_b#
  :: Limb              -- ^ minuend
  -> Limb              -- ^ subtrahend
  -> Limb              -- ^ borrow
  -> (# Limb, Limb #)  -- ^ (# difference, new borrow #)
sub_b# (Limb m) (Limb n) (Limb b) =
  let !(# d0, b0 #) = Exts.subWordC# m n
      !(#  d, b1 #) = Exts.subWordC# d0 b
      !c = Exts.int2Word# (Exts.negateInt# (Exts.orI# b0 b1))
  in  (# Limb d, Limb c #)
{-# INLINE sub_b# #-}

-- | Saturating subtraction, computing minuend - subtrahend, returning the
--   difference (and clamping to zero in the case of underflow).
sub_s#
  :: Limb -- ^ minuend
  -> Limb -- ^ subtrahend
  -> Limb -- ^ difference
sub_s# (Limb m) (Limb n) = case Exts.subWordC# m n of
  (# d, 0# #) -> Limb d
  _ -> Limb 0##
{-# INLINE sub_s# #-}

-- | Wrapping subtraction, computing minuend - subtrahend, returning the
--   difference (and discarding underflow).
sub_w#
  :: Limb -- ^ minuend
  -> Limb -- ^ subtrahend
  -> Limb -- ^ difference
sub_w# (Limb m) (Limb n) = Limb (Exts.minusWord# m n)
{-# INLINE sub_w# #-}

-- multiplication -------------------------------------------------------------

-- | Widening multiplication, returning low and high words of the product.
mul_c#
  :: Limb             -- ^ multiplicand
  -> Limb             -- ^ multiplier
  -> (# Limb, Limb #) -- ^ (# low, high #) product
mul_c# (Limb a) (Limb b) =
  let !(# h, l #) = Exts.timesWord2# a b
  in  (# Limb l, Limb h #)
{-# INLINE mul_c# #-}

-- | Wrapping multiplication, returning only the low word of the product.
mul_w#
  :: Limb -- ^ multiplicand
  -> Limb -- ^ multiplier
  -> Limb -- ^ low word of product
mul_w# (Limb a) (Limb b) = Limb (Exts.timesWord# a b)
{-# INLINE mul_w# #-}

-- | Saturating multiplication, returning only the low word of the product,
--   and clamping to the maximum value in the case of overflow.
mul_s#
  :: Limb -- ^ multiplicand
  -> Limb -- ^ multiplier
  -> Limb -- ^ clamped low word of product
mul_s# (Limb a) (Limb b) = case Exts.timesWord2# a b of
  (# 0##, l #) -> Limb l
  _ -> Limb (Exts.not# 0##)
{-# INLINE mul_s# #-}

-- | Multiply-add-carry, computing a * b + m + c, returning the
--   result along with the new carry.
mac#
  :: Limb              -- ^ a (multiplicand)
  -> Limb              -- ^ b (multiplier)
  -> Limb              -- ^ m (addend)
  -> Limb              -- ^ c (carry)
  -> (# Limb, Limb #)  -- ^ a * b + m + c
mac# (Limb a) (Limb b) (Limb m) (Limb c) =
    let !(# h, l #) = Exts.timesWord2# a b
        !(# l_0, h_0 #) = wadd_w# (# l, h #) m
        !(# d, l_1 #) = Exts.plusWord2# l_0 c
        !h_1 = Exts.plusWord# h_0 d
    in  (# Limb l_1, Limb h_1 #)
  where
    -- wide wrapping addition
    wadd_w# :: (# Word#, Word# #) -> Word# -> (# Word#, Word# #)
    wadd_w# (# x_lo, x_hi #) y_lo =
      let !(# c0, s0 #) = Exts.plusWord2# x_lo y_lo
          !(# _, s1 #) = Exts.plusWord2# x_hi c0
      in  (# s0, s1 #)
    {-# INLINE wadd_w# #-}
{-# INLINE mac# #-}

