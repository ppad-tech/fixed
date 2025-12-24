{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- |
-- Module: Data.Word.Wider
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Wider words, consisting of four 'Limb's.

module Data.Word.Wider (
  -- * Four-limb words
    Wider(..)
  , wider
  , to_vartime
  , from_vartime

  -- * Comparison
  , eq_vartime
  , cmp_vartime
  , cmp#
  , eq#
  , lt
  , lt#
  , gt
  , gt#

  -- * Parity
  , odd#
  , odd

  -- * Constant-time selection
  , select
  , select#

  -- * Bit manipulation
  , shl1
  , shr1
  , shl1_c
  , shr1_c
  , shr_limb
  , shl_limb
  , shl1_c#
  , shr1_c#
  , shr_limb#
  , shl_limb#
  , and
  , and_w#
  , or
  , or_w#
  , not
  , not#

  -- * Arithmetic
  , add_o
  , add_o#
  , add
  , add_w#
  , add_mod
  , add_mod#
  , sub
  , sub_b
  , sub_b#
  , sub_mod
  , sub_mod#
  , sub_mod_c#
  , mul
  , mul_c
  , mul_c#
  , sqr
  , sqr#
  ) where

import Control.DeepSeq
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import qualified Data.Bits as B
import qualified Data.Choice as C
import Data.Word.Limb (Limb(..))
import qualified Data.Word.Limb as L
import GHC.Exts (Word(..), Int(..), Word#, Int#)
import qualified GHC.Exts as Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip, odd)

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- wider words ----------------------------------------------------------------

pattern Limb4
  :: Word# -> Word# -> Word# -> Word#
  -> (# Limb, Limb, Limb, Limb #)
pattern Limb4 w0 w1 w2 w3 = (# Limb w0, Limb w1, Limb w2, Limb w3 #)
{-# COMPLETE Limb4 #-}

-- | Little-endian wider words, consisting of four 'Limbs'.
--
--   >>> 1 :: Wider
--   1
data Wider = Wider !(# Limb, Limb, Limb, Limb #)

instance Show Wider where
  show = show . from_vartime

-- | Note that 'fromInteger' necessarily runs in variable time due
--   to conversion from the variable-size, potentially heap-allocated
--   'Integer' type.
instance Num Wider where
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  fromInteger = to_vartime
  negate w = add (not w) (Wider (Limb4 1## 0## 0## 0##))
  signum (Wider (# l0, l1, l2, l3 #)) =
    let !(Limb l) = l0 `L.or#` l1 `L.or#` l2 `L.or#` l3
        !n = C.from_word_nonzero# l
        !b = C.to_word# n
    in  Wider (Limb4 b 0## 0## 0##)

instance NFData Wider where
  rnf (Wider a) = case a of
    (# _, _, _, _ #) -> ()

-- comparison -----------------------------------------------------------------

eq#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
  -> C.Choice
eq# a b =
  let !(Limb4 a0 a1 a2 a3) = a
      !(Limb4 b0 b1 b2 b3) = b
  in  C.eq_wider# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #)
{-# INLINE eq# #-}

-- | Compare 'Wider' words for equality in variable time.
--
--   >>> eq_vartime 1 0
--   False
--   >>> eq_vartime 1 1
--   True
eq_vartime :: Wider -> Wider -> Bool
eq_vartime a b =
  let !(Wider (# a0, a1, a2, a3 #)) = a
      !(Wider (# b0, b1, b2, b3 #)) = b
  in     (L.eq_vartime# a0 b0)
      && (L.eq_vartime# a1 b1)
      && (L.eq_vartime# a2 b2)
      && (L.eq_vartime# a3 b3)

lt#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
  -> C.Choice
lt# a b =
  let !(# _, Limb bor #) = sub_b# a b
  in  C.from_word_mask# bor
{-# INLINE lt# #-}

-- | Constant-time less-than comparison between 'Wider' values.
--
--   >>> import qualified Data.Choice as CT
--   >>> CT.decide (lt 1 2)
--   True
--   >>> CT.decide (lt 1 1)
--   False
lt :: Wider -> Wider -> C.Choice
lt (Wider a) (Wider b) = lt# a b

gt#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
  -> C.Choice
gt# a b =
  let !(# _, Limb bor #) = sub_b# b a
  in  C.from_word_mask# bor
{-# INLINE gt# #-}

-- | Constant-time greater-than comparison between 'Wider' values.
--
--   >>> import qualified Data.Choice as CT
--   >>> CT.decide (gt 1 2)
--   False
--   >>> CT.decide (gt 2 1)
--   True
gt :: Wider -> Wider -> C.Choice
gt (Wider a) (Wider b) = gt# a b

cmp#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
  -> Int#
cmp# (# l0, l1, l2, l3 #) (# r0, r1, r2, r3 #) =
  let !(# w0, b0 #) = L.sub_b# r0 l0 (Limb 0##)
      !d0           = L.or# (Limb 0##) w0
      !(# w1, b1 #) = L.sub_b# r1 l1 b0
      !d1           = L.or# d0 w1
      !(# w2, b2 #) = L.sub_b# r2 l2 b1
      !d2           = L.or# d1 w2
      !(# w3, b3 #) = L.sub_b# r3 l3 b2
      !d3           = L.or# d2 w3
      !(Limb w)     = L.and# b3 (Limb 2##)
      !s            = Exts.word2Int# w Exts.-# 1#
  in  (Exts.word2Int# (C.to_word# (L.nonzero# d3))) Exts.*# s
{-# INLINE cmp# #-}

-- | Variable-time comparison between 'Wider' words.
--
--   The actual comparison here is performed in constant time, but we must
--   branch to return an 'Ordering'.
--
--   >>> cmp_vartime 1 2
--   LT
--   >>> cmp_vartime 2 1
--   GT
--   >>> cmp_vartime 2 2
--   EQ
cmp_vartime :: Wider -> Wider -> Ordering
cmp_vartime (Wider a) (Wider b) = case cmp# a b of
  1#  -> GT
  0#  -> EQ
  _   -> LT
{-# INLINABLE cmp_vartime #-}

-- construction / conversion --------------------------------------------------

-- | Construct a 'Wider' word from four 'Words', provided in
--   little-endian order.
--
--   >>> wider 1 0 0 0
--   1
wider :: Word -> Word -> Word -> Word -> Wider
wider (W# w0) (W# w1) (W# w2) (W# w3) = Wider
  (# Limb w0, Limb w1, Limb w2, Limb w3 #)

-- | Convert an 'Integer' to a 'Wider' word.
--
--   >>> to_vartime 1
--   1
to_vartime :: Integer -> Wider
to_vartime n =
  let !size = B.finiteBitSize (0 :: Word)
      !mask = fi (maxBound :: Word) :: Integer
      !(W# w0) = fi (n .&. mask)
      !(W# w1) = fi ((n .>>. size) .&. mask)
      !(W# w2) = fi ((n .>>. (2 * size)) .&. mask)
      !(W# w3) = fi ((n .>>. (3 * size)) .&. mask)
  in  Wider (# Limb w0, Limb w1, Limb w2, Limb w3 #)

-- | Convert a 'Wider' word to an 'Integer'.
--
--   >>> from_vartime 1
--   1
from_vartime :: Wider -> Integer
from_vartime (Wider (# Limb w0, Limb w1, Limb w2, Limb w3 #)) =
        fi (W# w3) .<<. (3 * size)
    .|. fi (W# w2) .<<. (2 * size)
    .|. fi (W# w1) .<<. size
    .|. fi (W# w0)
  where
    !size = B.finiteBitSize (0 :: Word)

-- constant-time selection-----------------------------------------------------

select#
  :: (# Limb, Limb, Limb, Limb #) -- ^ a
  -> (# Limb, Limb, Limb, Limb #) -- ^ b
  -> C.Choice                     -- ^ c
  -> (# Limb, Limb, Limb, Limb #) -- ^ result
select# a b c =
  let !(# Limb a0, Limb a1, Limb a2, Limb a3 #) = a
      !(# Limb b0, Limb b1, Limb b2, Limb b3 #) = b
      !(# w0, w1, w2, w3 #) =
        C.select_wider# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) c
  in  (# Limb w0, Limb w1, Limb w2, Limb w3 #)
{-# INLINE select# #-}

-- | Return a if c is truthy, otherwise return b.
--
--   >>> import qualified Data.Choice as C
--   >>> select 0 1 (C.true# ())
--   1
select
  :: Wider    -- ^ a
  -> Wider    -- ^ b
  -> C.Choice -- ^ c
  -> Wider    -- ^ result
select (Wider a) (Wider b) c = Wider (select# a b c)

-- bit manipulation -----------------------------------------------------------

shr1_c#
  :: (# Limb, Limb, Limb, Limb #)                 -- ^ argument
  -> (# (# Limb, Limb, Limb, Limb #), C.Choice #) -- ^ result, carry
shr1_c# (# w0, w1, w2, w3 #) =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !(# s3, c3 #) = (# L.shr# w3 1#, L.shl# w3 s #)
      !r3           = L.or# s3 (Limb 0##)
      !(# s2, c2 #) = (# L.shr# w2 1#, L.shl# w2 s #)
      !r2           = L.or# s2 c3
      !(# s1, c1 #) = (# L.shr# w1 1#, L.shl# w1 s #)
      !r1           = L.or# s1 c2
      !(# s0, c0 #) = (# L.shr# w0 1#, L.shl# w0 s #)
      !r0           = L.or# s0 c1
      !(Limb w)     = L.shr# c0 s
  in  (# (# r0, r1, r2, r3 #), C.from_word# w #)
{-# INLINE shr1_c# #-}

-- | Constant-time 1-bit shift-right with carry, with a 'Choice'
--   indicating whether the lowest bit was set.
shr1_c :: Wider -> (# Wider, C.Choice #)
shr1_c (Wider w) =
  let !(# r, c #) = shr1_c# w
  in  (# Wider r, c #)

-- | Constant-time 1-bit shift-right.
--
--   >>> shr1 2
--   1
--   >>> shr1 1
--   0
shr1 :: Wider -> Wider
shr1 (Wider w) =
  let !(# r, _ #) = shr1_c# w
  in  Wider r

shl1_c#
  :: (# Limb, Limb, Limb, Limb #)                 -- ^ argument
  -> (# (# Limb, Limb, Limb, Limb #), C.Choice #) -- ^ result, carry
shl1_c# (# w0, w1, w2, w3 #) =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !(# s0, c0 #) = (# L.shl# w0 1#, L.shr# w0 s #)
      !r0           = L.or# s0 (Limb 0##)
      !(# s1, c1 #) = (# L.shl# w1 1#, L.shr# w1 s #)
      !r1           = L.or# s1 c0
      !(# s2, c2 #) = (# L.shl# w2 1#, L.shr# w2 s #)
      !r2           = L.or# s2 c1
      !(# s3, c3 #) = (# L.shl# w3 1#, L.shr# w3 s #)
      !r3           = L.or# s3 c2
      !(Limb w)     = L.shl# c3 s
  in  (# (# r0, r1, r2, r3 #), C.from_word# w #)
{-# INLINE shl1_c# #-}

-- | Constant-time 1-bit shift-left with carry, with a 'Choice' indicating
--   whether the highest bit was set.
shl1_c :: Wider -> (# Wider, C.Choice #)
shl1_c (Wider w) =
  let !(# r, c #) = shl1_c# w
  in  (# Wider r, c #)

-- | Constant-time 1-bit shift-left.
--
--   >>> shl1 1
--   2
--   >>> shl1 (2 ^ (255 :: Word))
--   0
shl1 :: Wider -> Wider
shl1 (Wider w) =
  let !(# r, _ #) = shl1_c# w
  in  Wider r

shr_limb#
  :: (# Limb, Limb, Limb, Limb #)
  -> Int#
  -> (# (# Limb, Limb, Limb, Limb #), Limb #)
shr_limb# (# a0, a1, a2, a3 #) rs =
  let !size = case B.finiteBitSize (0 :: Word) of I# m -> m
      !ls = size Exts.-# rs
      !(# l3, c3 #) = (# L.shr# a3 rs, L.shl# a3 ls #)
      !(# l2, c2 #) = (# L.or# (L.shr# a2 rs) c3, L.shl# a2 ls #)
      !(# l1, c1 #) = (# L.or# (L.shr# a1 rs) c2, L.shl# a1 ls #)
      !(# l0, c0 #) = (# L.or# (L.shr# a0 rs) c1, L.shl# a0 ls #)
  in  (# (# l0, l1, l2, l3 #), c0 #)
{-# INLINE shr_limb# #-}

-- | Shift right by less than the number of bits in a 'Limb' (e.g., by
--   a maximum of 63 bits on 64-bit architectures). The shift amount is
--   unchecked.
--
--   >>> shr_limb 2 1
--   1
shr_limb
  :: Wider -- ^ value
  -> Int   -- ^ right-shift amount (0 < s < WORD_SIZE)
  -> Wider -- ^ right-shifted value
shr_limb (Wider w) (I# s) =
  let !(# r, _ #) = shr_limb# w s
  in  Wider r

shl_limb#
  :: (# Limb, Limb, Limb, Limb #)
  -> Int#
  -> (# (# Limb, Limb, Limb, Limb #), Limb #)
shl_limb# (# a0, a1, a2, a3 #) ls =
  let !size = case B.finiteBitSize (0 :: Word) of I# m -> m
      !rs = size Exts.-# ls
      !(# l0, c0 #) = (# L.shl# a0 ls, L.shr# a0 rs #)
      !(# l1, c1 #) = (# L.or# (L.shl# a1 ls) c0, L.shr# a1 rs #)
      !(# l2, c2 #) = (# L.or# (L.shl# a2 ls) c1, L.shr# a2 rs #)
      !(# l3, c3 #) = (# L.or# (L.shl# a3 ls) c2, L.shr# a3 rs #)
  in  (# (# l0, l1, l2, l3 #), c3 #)
{-# INLINE shl_limb# #-}

-- | Shift left by less than the number of bits in a 'Limb' (e.g., by
--   a maximum of 63 bits on 64-bit architectures). The shift amount is
--   unchecked.
--
--   >>> shl_limb 2 1
--   1
--   >>> shl_limb 1 63
--   9223372036854775808
shl_limb
  :: Wider -- ^ value
  -> Int   -- ^ left-shift amount (0 < s < WORD_SIZE)
  -> Wider -- ^ left-shifted value
shl_limb (Wider w) (I# s) =
  let !(# r, _ #) = shl_limb# w s
  in  Wider r

and_w#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
and_w# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  (# L.and# a0 b0, L.and# a1 b1, L.and# a2 b2, L.and# a3 b3 #)
{-# INLINE and_w# #-}

-- | Binary /and/.
--
--   >>> and 1 1
--   1
--   >>> and 1 0
--   0
and
  :: Wider -- ^ a
  -> Wider -- ^ b
  -> Wider -- ^ a & b
and (Wider a) (Wider b) = Wider (and_w# a b)

or_w#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
or_w# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  (# L.or# a0 b0, L.or# a1 b1, L.or# a2 b2, L.or# a3 b3 #)
{-# INLINE or_w# #-}

-- | Binary /or/.
--
--   >>> or 1 1
--   1
--   >>> or 1 0
--   1
or
  :: Wider -- ^ a
  -> Wider -- ^ b
  -> Wider -- ^ a | b
or (Wider a) (Wider b) = Wider (or_w# a b)

not#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
not# (# l0, l1, l2, l3 #) = (# L.not# l0, L.not# l1, L.not# l2, L.not# l3 #)
{-# INLINE not# #-}

-- | Binary /not/.
--
--   >>> not 0
--   115792089237316195423570985008687907853269984665640564039457584007913129639935
--   >>> not (not 0)
--   0
not
  :: Wider -- ^ value
  -> Wider -- ^ not value
not (Wider w) = Wider (not# w)

-- addition, subtraction ------------------------------------------------------

add_o#
  :: (# Limb, Limb, Limb, Limb #)             -- ^ augend
  -> (# Limb, Limb, Limb, Limb #)             -- ^ addend
  -> (# (# Limb, Limb, Limb, Limb #), Limb #) -- ^ (# sum, carry bit #)
add_o# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  let !(# s0, c0 #) = L.add_o# a0 b0
      !(# s1, c1 #) = L.add_c# a1 b1 c0
      !(# s2, c2 #) = L.add_c# a2 b2 c1
      !(# s3, c3 #) = L.add_c# a3 b3 c2
  in  (# (# s0, s1, s2, s3 #), c3 #)
{-# INLINE add_o# #-}

-- | Overflowing addition, computing 'a + b', returning the sum and a
--   carry bit.
--
--   >>> add_o 1 1
--   (2,0)
--   >>> add_o 1 (2 ^ (256 :: Word) - 1)
--   (0,1)
add_o
  :: Wider          -- ^ augend
  -> Wider          -- ^ addend
  -> (Wider, Word)  -- ^ (sum, carry bit)
add_o (Wider a) (Wider b) =
  let !(# s, Limb c #) = add_o# a b
  in  (Wider s, W# c)

add_w#
  :: (# Limb, Limb, Limb, Limb #) -- ^ augend
  -> (# Limb, Limb, Limb, Limb #) -- ^ addend
  -> (# Limb, Limb, Limb, Limb #) -- ^ sum
add_w# a b =
  let !(# c, _ #) = add_o# a b
  in  c
{-# INLINE add_w# #-}

-- | Wrapping addition, computing 'a + b'.
--
--   Note that as 'Wider' is an instance of 'Num', you can use '+' to apply
--   this function.
--
--   >>> add 1 (2 ^ (256 :: Word) - 1)
--   0
add
  :: Wider
  -> Wider
  -> Wider
add (Wider a) (Wider b) = Wider (add_w# a b)
{-# INLINE add #-}

add_mod#
  :: (# Limb, Limb, Limb, Limb #) -- ^ augend
  -> (# Limb, Limb, Limb, Limb #) -- ^ addend
  -> (# Limb, Limb, Limb, Limb #) -- ^ modulus
  -> (# Limb, Limb, Limb, Limb #) -- ^ sum
add_mod# a b m =
  let !(# w, c #) = add_o# a b
  in  sub_mod_c# w c m m
{-# INLINE add_mod# #-}

-- | Modular addition.
--
--   Assumes that the sum is less than twice the modulus; this is not
--   checked.
--
--   >>> add_mod 1 1 3
--   2
--   >>> add_mod 1 2 3
--   0
add_mod
  :: Wider -- ^ augend
  -> Wider -- ^ addend
  -> Wider -- ^ modulus
  -> Wider -- ^ sum
add_mod (Wider a) (Wider b) (Wider m) = Wider (add_mod# a b m)

sub_b#
  :: (# Limb, Limb, Limb, Limb #)              -- ^ minuend
  -> (# Limb, Limb, Limb, Limb #)              -- ^ subtrahend
  -> (# (# Limb, Limb, Limb, Limb #), Limb #) -- ^ (# diff, borrow mask #)
sub_b# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  let !(# s0, c0 #) = L.sub_b# a0 b0 (Limb 0##)
      !(# s1, c1 #) = L.sub_b# a1 b1 c0
      !(# s2, c2 #) = L.sub_b# a2 b2 c1
      !(# s3, c3 #) = L.sub_b# a3 b3 c2
  in  (# (# s0, s1, s2, s3 #), c3 #)
{-# INLINE sub_b# #-}

-- | Borrowing subtraction, computing 'a - b' and returning the
--   difference with a borrow mask.
--
--   >>> sub_b 1 1
--   (0,0)
--   >>> sub_b 0 (2 ^ (256 :: Word) - 1)
--   (1,18446744073709551615)
sub_b
  :: Wider         -- ^ minuend
  -> Wider         -- ^ subtrahend
  -> (Wider, Word) -- ^ (difference, borrow mask)
sub_b (Wider l) (Wider r) =
  let !(# d, Limb b #) = sub_b# l r
  in  (Wider d, W# b)

-- | Wrapping subtraction, computing 'a - b' and returning the
--   difference.
--
--   Note that as 'Wider' is an instance of 'Num', you can use '-' to apply
--   this function.
--
--   >>> sub 1 1
--   0
--   >>> sub 0 (2 ^ (256 :: Word) - 1)
--   1
sub
  :: Wider -- ^ minuend
  -> Wider -- ^ subtrahend
  -> Wider -- ^ difference
sub (Wider a) (Wider b) =
  let !(# d, _ #) = sub_b# a b
  in  Wider d

sub_mod#
  :: (# Limb, Limb, Limb, Limb #) -- ^ minuend
  -> (# Limb, Limb, Limb, Limb #) -- ^ subtrahend
  -> (# Limb, Limb, Limb, Limb #) -- ^ modulus
  -> (# Limb, Limb, Limb, Limb #) -- ^ difference
sub_mod# a b (# p0, p1, p2, p3 #) =
  let !(# o, m #) = sub_b# a b
      !ba = (# L.and# p0 m, L.and# p1 m, L.and# p2 m, L.and# p3 m #)
  in  add_w# o ba
{-# INLINE sub_mod# #-}

-- | Modular subtraction. Computes a - b mod m.
--
--   Assumes that the magnitude of the difference is less than the
--   modulus (this is unchecked).
--
--   >>> sub_mod 1 1 4
--   0
--   >>> sub_mod 2 3 4
--   3
sub_mod
  :: Wider
  -> Wider
  -> Wider
  -> Wider
sub_mod (Wider a) (Wider b) (Wider p) = Wider (sub_mod# a b p)

-- | Modular subtraction with carry. Computes (# a, c #) - b mod m.
sub_mod_c#
  :: (# Limb, Limb, Limb, Limb #) -- ^ minuend
  -> Limb                         -- ^ carry bit
  -> (# Limb, Limb, Limb, Limb #) -- ^ subtrahend
  -> (# Limb, Limb, Limb, Limb #) -- ^ modulus
  -> (# Limb, Limb, Limb, Limb #) -- ^ difference
sub_mod_c# a c b (# p0, p1, p2, p3 #) =
  let !(# (# o0, o1, o2, o3 #), bb #) = sub_b# a b
      !(# _, m #) = L.sub_b# c (Limb 0##) bb
      !ba = (# L.and# p0 m, L.and# p1 m, L.and# p2 m, L.and# p3 m #)
  in  add_w# (# o0, o1, o2, o3 #) ba
{-# INLINE sub_mod_c# #-}

-- multiplication -------------------------------------------------------------

mul_c#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# Limb, Limb, Limb, Limb #)
  -> (# (# Limb, Limb, Limb, Limb #), (# Limb, Limb, Limb, Limb #) #)
mul_c# (# x0, x1, x2, x3 #) (# y0, y1, y2, y3 #) =
  let !(# z0, c0_0 #)   = L.mac# x0 y0 (Limb 0##) (Limb 0##)
      !(# s1_0, c1_0 #) = L.mac# x0 y1 (Limb 0##) c0_0
      !(# z1, c1_1 #)   = L.mac# x1 y0 s1_0 (Limb 0##)
      !(# s2_0, c2_0 #) = L.mac# x0 y2 (Limb 0##) c1_0
      !(# s2_1, c2_1 #) = L.mac# x1 y1 s2_0 c1_1
      !(# z2, c2_2 #)   = L.mac# x2 y0 s2_1 (Limb 0##)
      !(# s3_0, c3_0 #) = L.mac# x0 y3 (Limb 0##) c2_0
      !(# s3_1, c3_1 #) = L.mac# x1 y2 s3_0 c2_1
      !(# s3_2, c3_2 #) = L.mac# x2 y1 s3_1 c2_2
      !(# z3, c3_3 #)   = L.mac# x3 y0 s3_2 (Limb 0##)
      !(# s4_0, c4_0 #) = L.mac# x1 y3 (Limb 0##) c3_0
      !(# s4_1, c4_1 #) = L.mac# x2 y2 s4_0 c3_1
      !(# s4_2, c4_2 #) = L.mac# x3 y1 s4_1 c3_2
      !(# w4, c4_3 #)   = L.add_c# s4_2 c3_3 (Limb 0##)
      !(# s5_0, c5_0 #) = L.mac# x2 y3 (Limb 0##) c4_0
      !(# s5_1, c5_1 #) = L.mac# x3 y2 s5_0 c4_1
      !(# w5, c5_2 #)   = L.add_c# s5_1 c4_2 (Limb 0##)
      !(# w5f, c5_3 #)  = L.add_c# w5 c4_3 (Limb 0##)
      !(# s6_0, c6_0 #) = L.mac# x3 y3 (Limb 0##) c5_0
      !(# w6, c6_1 #)   = L.add_c# s6_0 c5_1 (Limb 0##)
      !(# w6f, c6_2 #)  = L.add_c# w6 c5_2 (Limb 0##)
      !(# w6ff, c6_3 #) = L.add_c# w6f c5_3 (Limb 0##)
      !(# w7, _ #)      = L.add_c# c6_0 c6_1 (Limb 0##)
      !(# w7f, _ #)     = L.add_c# w7 c6_2 (Limb 0##)
      !(# w7ff, _ #)    = L.add_c# w7f c6_3 (Limb 0##)
  in  (# (# z0, z1, z2, z3 #), (# w4, w5f, w6ff, w7ff #) #)
{-# INLINE mul_c# #-}

-- | Widening multiplication.
--
--   Returns the low and high 'Wider' words of the product, in that
--   order.
--
--   >>> mul_c 2 3
--   (6,0)
--   >>> mul_c (2 ^ (256 :: Word) - 1)  2
--   (115792089237316195423570985008687907853269984665640564039457584007913129639934,1)
mul_c
  :: Wider
  -> Wider
  -> (Wider, Wider)
mul_c (Wider a) (Wider b) =
  let !(# l, h #) = mul_c# a b
  in  (Wider l, Wider h)

-- | Wrapping multiplication.
--
--   Note that as 'Wider' is an instance of 'Num', you can use '*' to apply
--   this function.
--
--   >>> mul 1 1
--   1
--   >>> mul 1 2
--   2
mul
  :: Wider
  -> Wider
  -> Wider
mul (Wider a) (Wider b) =
  let !(# l, _ #) = mul_c# a b
  in  Wider l

sqr#
  :: (# Limb, Limb, Limb, Limb #)
  -> (# (# Limb, Limb, Limb, Limb #), (# Limb, Limb, Limb, Limb #) #)
sqr# (# x0, x1, x2, x3 #) =
  let !sh = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !(# q1_0, c1_0 #)  = L.mac# x1 x0 (Limb 0##) (Limb 0##)
      !r1                = c1_0
      !(# r2_0, c2_0 #)  = L.mac# x2 x0 r1 (Limb 0##)
      !(# s2_1, c2_1 #)  = L.mac# x2 x1 (Limb 0##) c2_0
      !t2                = c2_1
      !(# s3_0, c3_0 #)  = L.mac# x3 x0 s2_1 (Limb 0##)
      !(# t3, c3_1 #)    = L.mac# x3 x1 t2 c3_0
      !(# u3, c3_2 #)    = L.mac# x3 x2 (Limb 0##) c3_1
      !v3                = c3_2
      !(# lo1, car0_1 #) = (# L.shl# q1_0 1#, L.shr# q1_0 sh #)
      !(# lo2, car0_2 #) = (# L.or# (L.shl# r2_0 1#) car0_1, L.shr# r2_0 sh #)
      !(# lo3, car0_3 #) = (# L.or# (L.shl# s3_0 1#) car0_2, L.shr# s3_0 sh #)
      !(# hi0, car1_0 #) = (# L.or# (L.shl# t3 1#) car0_3, L.shr# t3 sh #)
      !(# hi1, car1_1 #) = (# L.or# (L.shl# u3 1#) car1_0, L.shr# u3 sh #)
      !(# hi2, car1_2 #) = (# L.or# (L.shl# v3 1#) car1_1, L.shr# v3 sh #)
      !hi3               = car1_2
      !(# pf, car2_0 #)  = L.mac# x0 x0 (Limb 0##) (Limb 0##)
      !(# qf, car2_1 #)  = L.add_c# lo1 car2_0 (Limb 0##)
      !(# rf, car2_2 #)  = L.mac# x1 x1 lo2 car2_1
      !(# sf, car2_3 #)  = L.add_c# lo3 car2_2 (Limb 0##)
      !(# tf, car2_4 #)  = L.mac# x2 x2 hi0 car2_3
      !(# uf, car2_5 #)  = L.add_c# hi1 car2_4 (Limb 0##)
      !(# vf, car2_6 #)  = L.mac# x3 x3 hi2 car2_5
      !(# wf, _      #)  = L.add_c# hi3 car2_6 (Limb 0##)
  in  (# (# pf, qf, rf, sf #), (# tf, uf, vf, wf #) #)
{-# INLINE sqr# #-}

-- | Widening square.
--
--   >>> sqr 2
--   (4,0)
--   >>> sqr (2 ^ (256 :: Word) - 1)
--   (1,115792089237316195423570985008687907853269984665640564039457584007913129639934)
sqr :: Wider -> (Wider, Wider)
sqr (Wider w) =
  let !(# l, h #) = sqr# w
  in  (Wider l, Wider h)

odd# :: (# Limb, Limb, Limb, Limb #) -> C.Choice
odd# (# Limb w, _, _, _ #) = C.from_word# (Exts.and# w 1##)
{-# INLINE odd# #-}

-- | Check if a 'Wider' is odd, returning a 'Choice'.
odd
  :: Wider
  -> C.Choice
odd (Wider w) = odd# w

