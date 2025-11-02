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

  -- * Arithmetic
  , add
  , sub
  , mul

  , add_w#
  , mul_w#
  ) where

import Control.DeepSeq
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

-- | Little-endian wide words.
data Wide = Wide (# Word#, Word# #)

get_lo# :: (# Word#, Word# #) -> Word#
get_lo# (# l, _ #) = l
{-# INLINE get_lo# #-}

-- | Get the low 'Word' from a 'Wide' word.
get_lo :: Wide -> Word
get_lo (Wide w) = W# (get_lo# w)

get_hi# :: (# Word#, Word# #) -> Word#
get_hi# (# _, h #) = h
{-# INLINE get_hi# #-}

-- | Get the high 'Word' from a 'Wide' word.
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

-- | Construct a 'Wide' word from a high 'Word'.
hi :: Word -> Wide
hi (W# w) = Wide (hi# w)

-- construct from lo
lo# :: Word# -> (# Word#, Word# #)
lo# w = (# w, 0## #)
{-# INLINE lo# #-}

-- | Construct a 'Wide' word from a low 'Word'.
lo :: Word -> Wide
lo (W# w) = Wide (lo# w)

-- | Construct a 'Wide' word from low and high 'Word's.
wide :: Word -> Word -> Wide
wide (W# l) (W# h) = Wide (# l, h #)

-- | Convert an 'Integer' to a 'Wide' word.
to :: Integer -> Wide
to n =
  let !size = B.finiteBitSize (0 :: Word)
      !mask = fi (maxBound :: Word) :: Integer
      !(W# w0) = fi (n .&. mask)
      !(W# w1) = fi ((n .>>. size) .&. mask)
  in  Wide (# w0, w1 #)

-- | Convert a 'Wide' word to an 'Integer'.
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

-- addition, subtraction ------------------------------------------------------

-- | Overflowing addition, computing 'a + b', returning the sum and a
--   carry bit.
add_c#
  :: (# Word#, Word# #)              -- ^ augend
  -> (# Word#, Word# #)              -- ^ addend
  -> (# (# Word#, Word# #), Word# #) -- ^ (# sum, carry bit #)
add_c# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.add_c# a0 b0 0##
      !(# s1, c1 #) = L.add_c# a1 b1 c0
  in  (# (# s0, s1 #), c1 #)
{-# INLINE add_c# #-}

-- | Wrapping addition, computing 'a + b'.
add_w#
  :: (# Word#, Word# #) -- ^ augend
  -> (# Word#, Word# #) -- ^ addend
  -> (# Word#, Word# #) -- ^ sum
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
  :: (# Word#, Word# #)              -- ^ minuend
  -> (# Word#, Word# #)              -- ^ subtrahend
  -> (# (# Word#, Word# #), Word# #) -- ^ (# difference, borrow bit #)
sub_b# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.sub_b# a0 b0 0##
      !(# s1, c1 #) = L.sub_b# a1 b1 c0
  in  (# (# s0, s1 #), c1 #)
{-# INLINE sub_b# #-}

-- | Wrapping subtraction, computing 'a - b'.
sub_w#
  :: (# Word#, Word# #) -- ^ minuend
  -> (# Word#, Word# #) -- ^ subtrahend
  -> (# Word#, Word# #) -- ^ difference
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
  :: (# Word#, Word# #) -- ^ multiplicand
  -> (# Word#, Word# #) -- ^ multiplier
  -> (# Word#, Word# #) -- ^ product
mul_w# (# a0, a1 #) (# b0, b1 #) =
  let !(# p0_lo, p0_hi #) = L.mul_c# a0 b0
      !(# p1_lo, _ #) = L.mul_c# a0 b1
      !(# p2_lo, _ #) = L.mul_c# a1 b0
      !(# s0, _ #) = L.add_c# p0_hi p1_lo 0##
      !(# s1, _ #) = L.add_c# s0 p2_lo 0##
  in  (# p0_lo, s1 #)
{-# INLINE mul_w# #-}

-- | Wrapping multiplication on 'Wide' words.
mul :: Wide -> Wide -> Wide
mul (Wide a) (Wide b) = Wide (mul_w# a b)

