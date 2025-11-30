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

  -- * Construction, Conversion
  , wide
  , to
  , from

  -- * Bit Manipulation
  , or
  , and
  , xor
  , not

  -- * Arithmetic
  , add
  , sub
  , mul

  -- * Unboxed Arithmetic
  , add_c#
  , add_w#
  , sub_b#
  , sub_w#
  , mul_w#
  ) where

import Control.DeepSeq
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import qualified Data.Bits as B
import Data.Word.Limb (Limb(..))
import qualified Data.Word.Limb as L
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- wide words -----------------------------------------------------------------

-- | Little-endian wide words.
data Wide = Wide !(# Limb, Limb #)

instance Show Wide where
  show = show . from

instance Num Wide where
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  fromInteger = to
  negate w = add (not w) (Wide (# Limb 1##, Limb 0## #))
  signum a = case a of
    Wide (# Limb 0##, Limb 0## #) -> 0
    _ -> 1

instance NFData Wide where
  rnf (Wide a) = case a of (# _, _ #) -> ()

-- construction / conversion --------------------------------------------------

-- | Construct a 'Wide' word from low and high 'Word's.
wide :: Word -> Word -> Wide
wide (W# l) (W# h) = Wide (# Limb l, Limb h #)

-- | Convert an 'Integer' to a 'Wide' word.
to :: Integer -> Wide
to n =
  let !size = B.finiteBitSize (0 :: Word)
      !mask = fi (maxBound :: Word) :: Integer
      !(W# w0) = fi (n .&. mask)
      !(W# w1) = fi ((n .>>. size) .&. mask)
  in  Wide (# Limb w0, Limb w1 #)

-- | Convert a 'Wide' word to an 'Integer'.
from :: Wide -> Integer
from (Wide (# Limb a, Limb b #)) =
      fi (W# b) .<<. (B.finiteBitSize (0 :: Word))
  .|. fi (W# a)

-- bits -----------------------------------------------------------------------

or_w# :: (# Limb, Limb #) -> (# Limb, Limb #) -> (# Limb, Limb #)
or_w# (# a0, a1 #) (# b0, b1 #) = (# L.or# a0 b0, L.or# a1 b1 #)
{-# INLINE or_w# #-}

or :: Wide -> Wide -> Wide
or (Wide a) (Wide b) = Wide (or_w# a b)

and_w# :: (# Limb, Limb #) -> (# Limb, Limb #) -> (# Limb, Limb #)
and_w# (# a0, a1 #) (# b0, b1 #) = (# L.and# a0 b0, L.and# a1 b1 #)
{-# INLINE and_w# #-}

and :: Wide -> Wide -> Wide
and (Wide a) (Wide b) = Wide (and_w# a b)

xor_w# :: (# Limb, Limb #) -> (# Limb, Limb #) -> (# Limb, Limb #)
xor_w# (# a0, a1 #) (# b0, b1 #) = (# L.xor# a0 b0, L.xor# a1 b1 #)
{-# INLINE xor_w# #-}

xor :: Wide -> Wide -> Wide
xor (Wide a) (Wide b) = Wide (xor_w# a b)

not_w# :: (# Limb, Limb #) -> (# Limb, Limb #)
not_w# (# a0, a1 #) = (# L.not# a0, L.not# a1 #)
{-# INLINE not_w# #-}

not :: Wide -> Wide
not (Wide w) = Wide (not_w# w)
{-# INLINE not #-}

-- addition, subtraction ------------------------------------------------------

-- | Overflowing addition, computing 'a + b', returning the sum and a
--   carry bit.
add_c#
  :: (# Limb, Limb #)              -- ^ augend
  -> (# Limb, Limb #)              -- ^ addend
  -> (# (# Limb, Limb #), Limb #) -- ^ (# sum, carry bit #)
add_c# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.add_o# a0 b0
      !(# s1, c1 #) = L.add_c# a1 b1 c0
  in  (# (# s0, s1 #), c1 #)
{-# INLINE add_c# #-}

-- | Wrapping addition, computing 'a + b'.
add_w#
  :: (# Limb, Limb #) -- ^ augend
  -> (# Limb, Limb #) -- ^ addend
  -> (# Limb, Limb #) -- ^ sum
add_w# a b =
  let !(# c, _ #) = add_c# a b
  in  c
{-# INLINE add_w# #-}

-- | Wrapping addition on 'Wide' words, computing 'a + b'.
add :: Wide -> Wide -> Wide
add (Wide a) (Wide b) = Wide (add_w# a b)

-- | Borrowing subtraction, computing 'a - b' and returning the
--   difference with a borrow bit.
sub_b#
  :: (# Limb, Limb #)              -- ^ minuend
  -> (# Limb, Limb #)              -- ^ subtrahend
  -> (# (# Limb, Limb #), Limb #) -- ^ (# difference, borrow bit #)
sub_b# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.sub_b# a0 b0 (Limb 0##)
      !(# s1, c1 #) = L.sub_b# a1 b1 c0
  in  (# (# s0, s1 #), c1 #)
{-# INLINE sub_b# #-}

-- | Wrapping subtraction, computing 'a - b'.
sub_w#
  :: (# Limb, Limb #) -- ^ minuend
  -> (# Limb, Limb #) -- ^ subtrahend
  -> (# Limb, Limb #) -- ^ difference
sub_w# a b =
  let !(# c, _ #) = sub_b# a b
  in  c
{-# INLINE sub_w# #-}

-- | Wrapping subtraction on 'Wide' words, computing 'a - b'.
sub :: Wide -> Wide -> Wide
sub (Wide a) (Wide b) = Wide (sub_w# a b)

-- multiplication -------------------------------------------------------------

-- | Wrapping multiplication, computing 'a b'.
mul_w#
  :: (# Limb, Limb #) -- ^ multiplicand
  -> (# Limb, Limb #) -- ^ multiplier
  -> (# Limb, Limb #) -- ^ product
mul_w# (# a0, a1 #) (# b0, b1 #) =
  let !(# p0_lo, p0_hi #) = L.mul_c# a0 b0
      !(# p1_lo, _ #) = L.mul_c# a0 b1
      !(# p2_lo, _ #) = L.mul_c# a1 b0
      !(# s0, _ #) = L.add_o# p0_hi p1_lo
      !(# s1, _ #) = L.add_o# s0 p2_lo
  in  (# p0_lo, s1 #)
{-# INLINE mul_w# #-}

-- | Wrapping multiplication on 'Wide' words.
mul :: Wide -> Wide -> Wide
mul (Wide a) (Wide b) = Wide (mul_w# a b)

