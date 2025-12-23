{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- |
-- Module: Data.Word.Wide
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Wide words, consisting of two 'Limb's.

module Data.Word.Wide (
  -- * Wide Words
    Wide(..)

  -- * Construction, Conversion
  , wide
  , to_vartime
  , from_vartime

  -- * Bit Manipulation
  , or
  , or#
  , and
  , and#
  , xor
  , xor#
  , not
  , not#

  -- * Comparison
  , eq_vartime

  -- * Arithmetic
  , add
  , add_o
  , sub
  , mul
  , neg

  -- * Unboxed Arithmetic
  , add_o#
  , add_w#
  , sub_b#
  , sub_w#
  , mul_w#
  , neg#
  ) where

import Control.DeepSeq
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import qualified Data.Bits as B
import qualified Data.Choice as C
import Data.Word.Limb (Limb(..))
import qualified Data.Word.Limb as L
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- wide words -----------------------------------------------------------------

pattern Limb2
  :: Word# -> Word#
  -> (# Limb, Limb #)
pattern Limb2 w0 w1 = (# Limb w0, Limb w1 #)
{-# COMPLETE Limb2 #-}

-- | Little-endian wide words.
data Wide = Wide !(# Limb, Limb #)

instance Show Wide where
  show = show . from_vartime

-- | Note that 'fromInteger' necessarily runs in variable time due
--   to conversion from the variable-size, potentially heap-allocated
--   'Integer' type.
instance Num Wide where
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  fromInteger = to_vartime
  negate = neg
  signum (Wide (# l0, l1 #)) =
    let !(Limb l) = l0 `L.or#` l1
        !n = C.from_word_nonzero# l
        !b = C.to_word# n
    in  Wide (Limb2 b 0##)

instance NFData Wide where
  rnf (Wide a) = case a of (# _, _ #) -> ()

-- construction / conversion --------------------------------------------------

-- | Construct a 'Wide' word from low and high 'Word's.
wide :: Word -> Word -> Wide
wide (W# l) (W# h) = Wide (# Limb l, Limb h #)

-- | Convert an 'Integer' to a 'Wide' word.
--
--   >>> to_vartime 1
--   1
to_vartime :: Integer -> Wide
to_vartime n =
  let !size = B.finiteBitSize (0 :: Word)
      !mask = fi (maxBound :: Word) :: Integer
      !(W# w0) = fi (n .&. mask)
      !(W# w1) = fi ((n .>>. size) .&. mask)
  in  Wide (# Limb w0, Limb w1 #)

-- | Convert a 'Wide' word to an 'Integer'.
--
--   >>> from_vartime 1
--   1
from_vartime :: Wide -> Integer
from_vartime (Wide (# Limb a, Limb b #)) =
      fi (W# b) .<<. (B.finiteBitSize (0 :: Word))
  .|. fi (W# a)

-- comparison -----------------------------------------------------------------

-- | Compare 'Wide' words for equality in variable time.
eq_vartime :: Wide -> Wide -> Bool
eq_vartime (Wide (# Limb a0, Limb b0 #)) (Wide (# Limb a1, Limb b1 #)) =
  isTrue# (andI# (eqWord# a0 a1) (eqWord# b0 b1))

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

-- negation -------------------------------------------------------------------

neg#
  :: (# Limb, Limb #) -- ^ argument
  -> (# Limb, Limb #) -- ^ (wrapping) additive inverse
neg# w = add_w# (not_w# w) (# Limb 1##, Limb 0## #)
{-# INLINE neg# #-}

neg
  :: Wide -- ^ argument
  -> Wide -- ^ (wrapping) additive inverse
neg (Wide w) = Wide (neg# w)

-- addition, subtraction ------------------------------------------------------

-- | Overflowing addition, computing 'a + b', returning the sum and a
--   carry bit.
add_o#
  :: (# Limb, Limb #)              -- ^ augend
  -> (# Limb, Limb #)              -- ^ addend
  -> (# (# Limb, Limb #), Limb #)  -- ^ (# sum, carry bit #)
add_o# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.add_o# a0 b0
      !(# s1, c1 #) = L.add_c# a1 b1 c0
  in  (# (# s0, s1 #), c1 #)
{-# INLINE add_o# #-}

-- | Overflowing addition on 'Wide' words, computing 'a + b', returning
--   the sum and carry.
add_o
  :: Wide         -- ^ augend
  -> Wide         -- ^ addend
  -> (Wide, Word) -- ^ (sum, carry)
add_o (Wide a) (Wide b) =
  let !(# s, Limb c #) = add_o# a b
  in  (Wide s, W# c)

-- | Wrapping addition, computing 'a + b'.
add_w#
  :: (# Limb, Limb #) -- ^ augend
  -> (# Limb, Limb #) -- ^ addend
  -> (# Limb, Limb #) -- ^ sum
add_w# a b =
  let !(# c, _ #) = add_o# a b
  in  c
{-# INLINE add_w# #-}

-- | Wrapping addition on 'Wide' words, computing 'a + b'.
add :: Wide -> Wide -> Wide
add (Wide a) (Wide b) = Wide (add_w# a b)

-- | Borrowing subtraction, computing 'a - b' and returning the
--   difference with a borrow mask.
sub_b#
  :: (# Limb, Limb #)              -- ^ minuend
  -> (# Limb, Limb #)              -- ^ subtrahend
  -> (# (# Limb, Limb #), Limb #) -- ^ (# difference, borrow mask #)
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

