{-# OPTIONS_HADDOCK prune #-}
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

  -- * Constant-time selection
  , select
  , select#

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
  , eq
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
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- wide words -----------------------------------------------------------------

type Limb2 = (# Limb, Limb #)

pattern L2 :: L.Word# -> L.Word# -> Limb2
pattern L2 w0 w1 = (# Limb w0, Limb w1 #)
{-# COMPLETE L2 #-}

-- | Little-endian wide words.
data Wide = Wide !Limb2

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
    in  Wide (L2 b 0##)

instance NFData Wide where
  rnf (Wide a) = case a of (# _, _ #) -> ()

-- construction / conversion --------------------------------------------------

-- | Construct a 'Wide' word from low and high 'Word's.
wide :: Word -> Word -> Wide
wide (L.W# l) (L.W# h) = Wide (# Limb l, Limb h #)
{-# INLINE wide #-}

-- | Convert an 'Integer' to a 'Wide' word in variable time.
--
--   >>> to_vartime 1
--   1
to_vartime :: Integer -> Wide
to_vartime n =
  let !size = B.finiteBitSize (0 :: Word)
      !mask = fi (maxBound :: Word) :: Integer
      !(L.W# w0) = fi (n .&. mask)
      !(L.W# w1) = fi ((n .>>. size) .&. mask)
  in  Wide (# Limb w0, Limb w1 #)
{-# INLINABLE to_vartime #-}

-- | Convert a 'Wide' word to an 'Integer' in variable time.
--
--   >>> from_vartime 1
--   1
from_vartime :: Wide -> Integer
from_vartime (Wide (# Limb a, Limb b #)) =
      fi (L.W# b) .<<. (B.finiteBitSize (0 :: Word))
  .|. fi (L.W# a)
{-# INLINABLE from_vartime #-}

-- comparison -----------------------------------------------------------------

-- | Compare 'Wide' words for equality in constant time.
--
--   >>> import qualified Data.Chocie as C
--   >>> C.decide (eq 1 1)
--   True
eq :: Wide -> Wide -> C.Choice
eq (Wide (# Limb a0, Limb a1 #)) (Wide (# Limb b0, Limb b1 #)) =
  C.eq_wide# (# a0, a1 #) (# b0, b1 #)
{-# INLINABLE eq #-}

-- | Compare 'Wide' words for equality in variable time.
--
--   >>> eq_vartime 1 1
--   True
eq_vartime :: Wide -> Wide -> Bool
eq_vartime (Wide (# a0, b0 #)) (Wide (# a1, b1 #)) =
  L.eq_vartime# a0 a1 && L.eq_vartime# b0 b1
{-# INLINABLE eq_vartime #-}

-- constant-time selection-----------------------------------------------------

-- | Return a if c is truthy, otherwise return b.
--
--   >>> import qualified Data.Choice as C
--   >>> select 0 1 (C.true# ())
--   1
select
  :: Wide     -- ^ a
  -> Wide     -- ^ b
  -> C.Choice -- ^ c
  -> Wide     -- ^ result
select (Wide a) (Wide b) c = Wide (select# a b c)
{-# INLINABLE select #-}

select#
  :: Limb2    -- ^ a
  -> Limb2    -- ^ b
  -> C.Choice -- ^ c
  -> Limb2    -- ^ result
select# (L2 a0 a1) (L2 b0 b1) c =
  let !(# w0, w1 #) = C.select_wide# (# a0, a1 #) (# b0, b1 #) c
  in  L2 w0 w1
{-# INLINE select# #-}

-- bits -----------------------------------------------------------------------

or# :: Limb2 -> Limb2 -> Limb2
or# (# a0, a1 #) (# b0, b1 #) = (# L.or# a0 b0, L.or# a1 b1 #)
{-# INLINE or# #-}

-- | Logical disjunction on 'Wide' words.
or :: Wide -> Wide -> Wide
or (Wide a) (Wide b) = Wide (or# a b)
{-# INLINABLE or #-}

and# :: Limb2 -> Limb2 -> Limb2
and# (# a0, a1 #) (# b0, b1 #) = (# L.and# a0 b0, L.and# a1 b1 #)
{-# INLINE and# #-}

-- | Logical conjunction on 'Wide' words.
and :: Wide -> Wide -> Wide
and (Wide a) (Wide b) = Wide (and# a b)
{-# INLINABLE and #-}

xor# :: Limb2 -> Limb2 -> Limb2
xor# (# a0, a1 #) (# b0, b1 #) = (# L.xor# a0 b0, L.xor# a1 b1 #)
{-# INLINE xor# #-}

-- | Logical exclusive-or on 'Wide' words.
xor :: Wide -> Wide -> Wide
xor (Wide a) (Wide b) = Wide (xor# a b)
{-# INLINABLE xor #-}

not# :: Limb2 -> Limb2
not# (# a0, a1 #) = (# L.not# a0, L.not# a1 #)
{-# INLINE not# #-}

-- | Logical negation on 'Wide' words.
not :: Wide -> Wide
not (Wide w) = Wide (not# w)
{-# INLINABLE not #-}

-- negation -------------------------------------------------------------------

-- | Wrapping negation on 'Wide' words, producing an additive inverse.
--
--   >>> neg 1
--   340282366920938463463374607431768211455
--   >>> 1 + neg 1
--   >>> 0
neg
  :: Wide -- ^ argument
  -> Wide -- ^ (wrapping) additive inverse
neg (Wide w) = Wide (neg# w)
{-# INLINABLE neg #-}

neg#
  :: Limb2 -- ^ argument
  -> Limb2 -- ^ (wrapping) additive inverse
neg# w = add_w# (not# w) (L2 1## 0##)
{-# INLINE neg# #-}

-- addition, subtraction ------------------------------------------------------

-- | Overflowing addition, computing 'a + b', returning the sum and a
--   carry bit.
add_o#
  :: Limb2              -- ^ augend
  -> Limb2              -- ^ addend
  -> (# Limb2, Limb #)  -- ^ (# sum, carry bit #)
add_o# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.add_o# a0 b0
      !(# s1, c1 #) = L.add_c# a1 b1 c0
  in  (# (# s0, s1 #), c1 #)
{-# INLINE add_o# #-}

-- | Overflowing addition on 'Wide' words, computing 'a + b', returning
--   the sum and carry bit.
add_o
  :: Wide         -- ^ augend
  -> Wide         -- ^ addend
  -> (Wide, Word) -- ^ (sum, carry)
add_o (Wide a) (Wide b) =
  let !(# s, Limb c #) = add_o# a b
  in  (Wide s, L.W# c)

-- | Wrapping addition, computing 'a + b'.
add_w#
  :: Limb2 -- ^ augend
  -> Limb2 -- ^ addend
  -> Limb2 -- ^ sum
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
  :: Limb2              -- ^ minuend
  -> Limb2              -- ^ subtrahend
  -> (# Limb2, Limb #) -- ^ (# difference, borrow mask #)
sub_b# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = L.sub_b# a0 b0 (Limb 0##)
      !(# s1, c1 #) = L.sub_b# a1 b1 c0
  in  (# (# s0, s1 #), c1 #)
{-# INLINE sub_b# #-}

-- | Wrapping subtraction, computing 'a - b'.
sub_w#
  :: Limb2 -- ^ minuend
  -> Limb2 -- ^ subtrahend
  -> Limb2 -- ^ difference
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
  :: Limb2 -- ^ multiplicand
  -> Limb2 -- ^ multiplier
  -> Limb2 -- ^ product
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

