{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- |
-- Module: Data.Word.Limb
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- The primitive 'Limb' type, as well as operations on it.

module Data.Word.Limb (
  -- * Limb
    Limb(..)
  , render

  -- * Bit manipulation and representation
  , and#
  , or#
  , not#
  , xor#
  , bits#
  , shl#
  , shl1#
  , shr#
  , shr1#

  -- * Comparison
  , eq#
  , ne#
  , eq_vartime#
  , ne_vartime#
  , nonzero#
  , lt#
  , gt#

  -- * Selection
  , select#
  , cswap#

  -- * Negation

  , neg#

  -- * Arithmetic
  , add_o#
  , add_c#
  , add_w#
  , add_s#

  , sub_b#
  , sub_w#
  , sub_s#

  , mul_c#
  , mul_w#
  , mul_s#

  , mac#
  ) where

import qualified Data.Bits as B
import qualified Data.Choice as C
import GHC.Exts (Word#)
import qualified GHC.Exts as Exts

-- | A 'Limb' is the smallest component of a wider word.
newtype Limb = Limb Word#

-- | Return a 'Limb' value as a 'String'.
render :: Limb -> String
render (Limb a) = show (Exts.W# a)

-- comparison -----------------------------------------------------------------

-- | Equality comparison.
eq#
  :: Limb
  -> Limb
  -> C.Choice
eq# (Limb a) (Limb b) = C.ct_eq_word# a b
{-# INLINE eq# #-}

eq_vartime#
  :: Limb
  -> Limb
  -> Bool
eq_vartime# (Limb a) (Limb b) = Exts.isTrue# (Exts.eqWord# a b)
{-# INLINE eq_vartime# #-}

-- | Inequality comparison.
ne#
  :: Limb
  -> Limb
  -> C.Choice
ne# a b = C.not# (eq# a b)
{-# INLINE ne# #-}

ne_vartime#
  :: Limb
  -> Limb
  -> Bool
ne_vartime# a b = not (eq_vartime# a b)
{-# INLINE ne_vartime# #-}

-- | Comparison to zero.
nonzero#
  :: Limb
  -> C.Choice
nonzero# (Limb a) = C.from_word_nonzero# a
{-# INLINE nonzero# #-}

-- | Less than.
lt#
  :: Limb
  -> Limb
  -> C.Choice
lt# (Limb a) (Limb b) = C.from_word_lt# a b
{-# INLINE lt# #-}

-- | Greater than.
gt#
  :: Limb
  -> Limb
  -> C.Choice
gt# (Limb a) (Limb b) = C.from_word_gt# a b
{-# INLINE gt# #-}

-- selection ------------------------------------------------------------------

-- | Return a if c is truthy, otherwise return b.
select#
  :: Limb     -- ^ a
  -> Limb     -- ^ b
  -> C.Choice -- ^ c
  -> Limb     -- ^ result
select# (Limb a) (Limb b) c = Limb (C.ct_select_word# a b c)
{-# INLINE select# #-}

-- | Return (# b, a #) if c is truthy, otherwise return (# a, b #).
cswap#
  :: Limb             -- ^ a
  -> Limb             -- ^ b
  -> C.Choice         -- ^ c
  -> (# Limb, Limb #) -- ^ result
cswap# (Limb a) (Limb b) c =
  let !l = C.ct_select_word# a b c
      !r = C.ct_select_word# b a c
  in  (# Limb l, Limb r #)
{-# INLINE cswap# #-}

-- bit manipulation -----------------------------------------------------------

-- | Bitwise and.
and#
  :: Limb -- ^ a
  -> Limb -- ^ b
  -> Limb -- ^ a & b
and# (Limb a) (Limb b) = Limb (Exts.and# a b)
{-# INLINE and# #-}

-- | Bitwise or.
or#
  :: Limb -- ^ a
  -> Limb -- ^ b
  -> Limb -- ^ a | b
or# (Limb a) (Limb b) = Limb (Exts.or# a b)
{-# INLINE or# #-}

-- | Bitwise not.
not#
  :: Limb -- ^ a
  -> Limb -- ^ not a
not# (Limb a) = Limb (Exts.not# a)
{-# INLINE not# #-}

-- | Bitwise exclusive or.
xor#
  :: Limb -- ^ a
  -> Limb -- ^ b
  -> Limb -- ^ a ^ b
xor# (Limb a) (Limb b) = Limb (Exts.xor# a b)
{-# INLINE xor# #-}

-- | Number of bits required to represent this limb.
bits#
  :: Limb -- ^ limb
  -> Int  -- ^ bits required to represent limb
bits# (Limb a) =
  let !_BITS = B.finiteBitSize (0 :: Word)
      !zs = B.countLeadingZeros (Exts.W# a)
  in  _BITS - zs -- XX unbox?
{-# INLINE bits# #-}

-- | Bit-shift left.
shl#
  :: Limb       -- ^ limb
  -> Exts.Int#  -- ^ shift amount
  -> Limb       -- ^ result
shl# (Limb w) s = Limb (Exts.uncheckedShiftL# w s)
{-# INLINE shl# #-}

-- | Bit-shift left by 1, returning the result and carry.
shl1#
  :: Limb
  -> (# Limb, Limb #)
shl1# (Limb w) =
  let !s = case B.finiteBitSize (0 :: Word) of Exts.I# m -> m Exts.-# 1#
      !r = Exts.uncheckedShiftL# w 1#
      !c = Exts.uncheckedShiftRL# w s
  in  (# Limb r, Limb c #)
{-# INLINE shl1# #-}

-- | Bit-shift right.
shr#
  :: Limb       -- ^ limb
  -> Exts.Int#  -- ^ shift amount
  -> Limb       -- ^ result
shr# (Limb w) s = Limb (Exts.uncheckedShiftRL# w s)
{-# INLINE shr# #-}

-- | Bit-shift right by 1, returning the result and carry.
shr1#
  :: Limb
  -> (# Limb, Limb #)
shr1# (Limb w) =
  let !s = case B.finiteBitSize (0 :: Word) of Exts.I# m -> m Exts.-# 1#
      !r = Exts.uncheckedShiftRL# w 1#
      !c = Exts.uncheckedShiftL# w s
  in  (# Limb r, Limb c #)
{-# INLINE shr1# #-}

-- negation -------------------------------------------------------------------

-- | Wrapping (two's complement) negation.
neg#
  :: Limb
  -> Limb
neg# (Limb x) = Limb (Exts.plusWord# (Exts.not# x) 1##)
{-# INLINE neg# #-}

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
sub_b# (Limb m) (Limb n) (Limb a) =
  let !s = case B.finiteBitSize (0 :: Word) of Exts.I# bs -> bs Exts.-# 1#
      !b = Exts.uncheckedShiftRL# a s
      !(# d0, b0 #) = Exts.subWordC# m n
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

