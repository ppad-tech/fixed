{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Word.Wider where

import Control.DeepSeq
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import qualified Data.Choice as C
import qualified Data.Bits as B
import qualified Data.Word.Limb as L
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- bit to mask (wrapping negation)
wrapping_neg# :: Word# -> Word#
wrapping_neg# w = plusWord# (not# w) 1##
{-# INLINE wrapping_neg# #-}

-- wider words ----------------------------------------------------------------

-- | Little-endian wider words.
data Wider = Wider !(# Word#, Word#, Word#, Word# #)

instance Eq Wider where
  Wider a == Wider b = C.decide (C.ct_eq_wider# a b)

instance Ord Wider where
  compare (Wider a) (Wider b) = case cmp# a b of
    1#  -> GT
    0#  -> EQ
    _   -> LT

instance Show Wider where
  show (Wider (# a, b, c, d #)) =
       "(" <> show (W# a) <> ", " <> show (W# b) <> ", "
    <> show (W# c) <> ", " <> show (W# d) <> ")"

instance NFData Wider where
  rnf (Wider a) = case a of
    (# _, _, _, _ #) -> ()

instance Num Wider where
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  fromInteger = to
  negate = to . negate . from
  signum a
    | a == Wider (# 0##, 0##, 0##, 0## #) = 0
    | otherwise = 1

-- ordering -------------------------------------------------------------------

lt#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> C.Choice
lt# a b =
  let !(# _, bit #) = sub_b# a b
  in  C.from_word_lsb# bit

gt#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> C.Choice
gt# a b =
  let !(# _, bit #) = sub_b# b a
  in  C.from_word_lsb# bit

cmp#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> Int#
cmp# (# l0, l1, l2, l3 #) (# r0, r1, r2, r3 #) =
  let !(# w0, b0 #) = L.sub_b# r0 l0 0##
      !d0 = or# 0## w0
      !(# w1, b1 #) = L.sub_b# r1 l1 b0
      !d1 = or# d0 w1
      !(# w2, b2 #) = L.sub_b# r2 l2 b1
      !d2 = or# d1 w2
      !(# w3, b3 #) = L.sub_b# r3 l3 b2
      !d3 = or# d2 w3
      !s = (word2Int# (uncheckedShiftL# b3 1#)) -# 1#
  in  (word2Int# (C.to_word# (C.from_word_nonzero# d3))) *# s
{-# INLINE cmp# #-}

-- construction / conversion --------------------------------------------------

-- | Construct a 'Wider' word from four 'Words', provided in
--   little-endian order.
wider :: Word -> Word -> Word -> Word -> Wider
wider (W# w0) (W# w1) (W# w2) (W# w3) = Wider (# w0, w1, w2, w3 #)

-- | Convert an 'Integer' to a 'Wider' word.
to :: Integer -> Wider
to n =
  let !size = B.finiteBitSize (0 :: Word)
      !mask = fi (maxBound :: Word) :: Integer
      !(W# w0) = fi (n .&. mask)
      !(W# w1) = fi ((n .>>. size) .&. mask)
      !(W# w2) = fi ((n .>>. (2 * size)) .&. mask)
      !(W# w3) = fi ((n .>>. (3 * size)) .&. mask)
  in  Wider (# w0, w1, w2, w3 #)

-- | Convert a 'Wider' word to an 'Integer'.
from :: Wider -> Integer
from (Wider (# w0, w1, w2, w3 #)) =
        fi (W# w3) .<<. (3 * size)
    .|. fi (W# w2) .<<. (2 * size)
    .|. fi (W# w1) .<<. size
    .|. fi (W# w0)
  where
    !size = B.finiteBitSize (0 :: Word)

-- bit manipulation -----------------------------------------------------------

-- | Constant-time 1-bit shift-right with carry, indicating whether the
--   lowest bit was set.
shr1_c#
  :: (# Word#, Word#, Word#, Word# #)                 -- ^ argument
  -> (# (# Word#, Word#, Word#, Word# #), C.Choice #) -- ^ result, carry
shr1_c# (# w0, w1, w2, w3 #) =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !c = 0##
      !(# s3, c3 #) = (# uncheckedShiftRL# w3 1#, uncheckedShiftL# w3 s #)
      !r3 = or# s3 c
      !(# s2, c2 #) = (# uncheckedShiftRL# w2 1#, uncheckedShiftL# w2 s #)
      !r2 = or# s2 c3
      !(# s1, c1 #) = (# uncheckedShiftRL# w1 1#, uncheckedShiftL# w1 s #)
      !r1 = or# s1 c2
      !(# s0, c0 #) = (# uncheckedShiftRL# w0 1#, uncheckedShiftL# w0 s #)
      !r0 = or# s0 c1
  in  (# (# r0, r1, r2, r3 #), C.from_word_lsb# (uncheckedShiftRL# c0 s) #)
{-# INLINE shr1_c# #-}

shr1_c :: Wider -> (Wider, Bool)
shr1_c (Wider w) =
  let !(# r, c #) = shr1_c# w
  in  (Wider r, C.decide c)

and_w#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
and_w# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  (# and# a0 b0, and# a1 b1, and# a2 b2, and# a3 b3 #)
{-# INLINE and_w# #-}

and :: Wider -> Wider -> Wider
and (Wider a) (Wider b) = Wider (and_w# a b)

or_w#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
or_w# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  (# or# a0 b0, or# a1 b1, or# a2 b2, or# a3 b3 #)
{-# INLINE or_w# #-}

or :: Wider -> Wider -> Wider
or (Wider a) (Wider b) = Wider (or_w# a b)

-- conditional_shr#
--   :: (# Word#, Word#, Word#, Word# #)
--   -> Int#
--   -> C.Choice
--   -> (# (# Word#, Word#, Word#, Word# #), Word# #)
-- conditional_shr# (# a0, a1, a2, a3 #) s c =
--   let !size = case B.finiteBitSize (0 :: Word) of I# m -> m
--       !rs = s
--       !ls = size -# s
--       !(# l3, c3 #) =
--         (# C.ct_select_word# a3 (uncheckedShiftRL# a3 rs)

-- addition, subtraction ------------------------------------------------------

-- | Overflowing addition, computing 'a + b', returning the sum and a
--   carry bit.
add_c#
  :: (# Word#, Word#, Word#, Word# #)              -- ^ augend
  -> (# Word#, Word#, Word#, Word# #)              -- ^ addend
  -> (# (# Word#, Word#, Word#, Word# #), Word# #) -- ^ (# sum, carry bit #)
add_c# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  let !(# c0, s0 #) = plusWord2# a0 b0
      !(# s1, c1 #) = L.add_c# a1 b1 c0
      !(# s2, c2 #) = L.add_c# a2 b2 c1
      !(# s3, c3 #) = L.add_c# a3 b3 c2
  in  (# (# s0, s1, s2, s3 #), c3 #)
{-# INLINE add_c# #-}

-- | Wrapping addition, computing 'a + b'.
add_w#
  :: (# Word#, Word#, Word#, Word# #) -- ^ augend
  -> (# Word#, Word#, Word#, Word# #) -- ^ addend
  -> (# Word#, Word#, Word#, Word# #) -- ^ sum
add_w# a b =
  let !(# c, _ #) = add_c# a b
  in  c
{-# INLINE add_w# #-}

-- | Wrapping addition, computing 'a + b'.
add
  :: Wider
  -> Wider
  -> Wider
add (Wider a) (Wider b) = Wider (add_w# a b)
{-# INLINE add #-}

-- | Modular addition.
add_mod#
  :: (# Word#, Word#, Word#, Word# #) -- ^ augend
  -> (# Word#, Word#, Word#, Word# #) -- ^ addend
  -> (# Word#, Word#, Word#, Word# #) -- ^ modulus
  -> (# Word#, Word#, Word#, Word# #) -- ^ sum
add_mod# a b m =
  let !(# w, c #) = add_c# a b
  in  sub_mod_c# w c m m
{-# INLINE add_mod# #-}

-- | Borrowing subtraction, computing 'a - b' and returning the
--   difference with a borrow bit.
sub_b#
  :: (# Word#, Word#, Word#, Word# #)              -- ^ minuend
  -> (# Word#, Word#, Word#, Word# #)              -- ^ subtrahend
  -> (# (# Word#, Word#, Word#, Word# #), Word# #) -- ^ (# diff, borrow bit #)
sub_b# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  let !(# s0, c0 #) = L.sub_b# a0 b0 0##
      !(# s1, c1 #) = L.sub_b# a1 b1 c0
      !(# s2, c2 #) = L.sub_b# a2 b2 c1
      !(# s3, c3 #) = L.sub_b# a3 b3 c2
  in  (# (# s0, s1, s2, s3 #), c3 #)
{-# INLINE sub_b# #-}

sub
  :: Wider
  -> Wider
  -> Wider
sub (Wider a) (Wider b) =
  let !(# d, _ #) = sub_b# a b
  in  Wider d

-- | Modular subtraction. Computes a - b mod m.
sub_mod#
  :: (# Word#, Word#, Word#, Word# #) -- ^ minuend
  -> (# Word#, Word#, Word#, Word# #) -- ^ subtrahend
  -> (# Word#, Word#, Word#, Word# #) -- ^ modulus
  -> (# Word#, Word#, Word#, Word# #) -- ^ difference
sub_mod# a b (# p0, p1, p2, p3 #) =
  let !(# (# o0, o1, o2, o3 #), bb #) = sub_b# a b
      !mask = wrapping_neg# bb
      !band = (# and# p0 mask, and# p1 mask, and# p2 mask, and# p3 mask #)
  in  add_w# (# o0, o1, o2, o3 #) band
{-# INLINE sub_mod# #-}

-- | Modular subtraction with carry. Computes (# a, c #) - b mod m.
sub_mod_c#
  :: (# Word#, Word#, Word#, Word# #) -- ^ minuend
  -> Word#                            -- ^ carry bit
  -> (# Word#, Word#, Word#, Word# #) -- ^ subtrahend
  -> (# Word#, Word#, Word#, Word# #) -- ^ modulus
  -> (# Word#, Word#, Word#, Word# #) -- ^ difference
sub_mod_c# a c b (# p0, p1, p2, p3 #) =
  let !(# (# o0, o1, o2, o3 #), bb #) = sub_b# a b
      !mask = and# (not# (wrapping_neg# c)) (wrapping_neg# bb)
      !band = (# and# p0 mask, and# p1 mask, and# p2 mask, and# p3 mask #)
  in  add_w# (# o0, o1, o2, o3 #) band
{-# INLINE sub_mod_c# #-}

-- multiplication -------------------------------------------------------------

-- widening multiplication
mul_c#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> (# (# Word#, Word#, Word#, Word# #), (# Word#, Word#, Word#, Word# #) #)
mul_c# (# x0, x1, x2, x3 #) (# y0, y1, y2, y3 #) =
  let !(# z0, c0_0 #)   = L.mac# x0 y0 0## 0##
      !(# s1_0, c1_0 #) = L.mac# x0 y1 0## c0_0
      !(# z1, c1_1 #)   = L.mac# x1 y0 s1_0 0##
      !(# s2_0, c2_0 #) = L.mac# x0 y2 0## c1_0
      !(# s2_1, c2_1 #) = L.mac# x1 y1 s2_0 c1_1
      !(# z2, c2_2 #)   = L.mac# x2 y0 s2_1 0##
      !(# s3_0, c3_0 #) = L.mac# x0 y3 0## c2_0
      !(# s3_1, c3_1 #) = L.mac# x1 y2 s3_0 c2_1
      !(# s3_2, c3_2 #) = L.mac# x2 y1 s3_1 c2_2
      !(# z3, c3_3 #)   = L.mac# x3 y0 s3_2 0##
      !(# s4_0, c4_0 #) = L.mac# x1 y3 0## c3_0
      !(# s4_1, c4_1 #) = L.mac# x2 y2 s4_0 c3_1
      !(# s4_2, c4_2 #) = L.mac# x3 y1 s4_1 c3_2
      !(# w4, c4_3 #)   = L.add_c# s4_2 c3_3 0##
      !(# s5_0, c5_0 #) = L.mac# x2 y3 0## c4_0
      !(# s5_1, c5_1 #) = L.mac# x3 y2 s5_0 c4_1
      !(# w5, c5_2 #)   = L.add_c# s5_1 c4_2 0##
      !(# w5f, c5_3 #)  = L.add_c# w5 c4_3 0##
      !(# s6_0, c6_0 #) = L.mac# x3 y3 0## c5_0
      !(# w6, c6_1 #)   = L.add_c# s6_0 c5_1 0##
      !(# w6f, c6_2 #)  = L.add_c# w6 c5_2 0##
      !(# w6ff, c6_3 #) = L.add_c# w6f c5_3 0##
      !(# w7, _ #)      = L.add_c# c6_0 c6_1 0##
      !(# w7f, _ #)     = L.add_c# w7 c6_2 0##
      !(# w7ff, _ #)    = L.add_c# w7f c6_3 0##
  in  (# (# z0, z1, z2, z3 #), (# w4, w5f, w6ff, w7ff #) #)
{-# INLINE mul_c# #-}

mul
  :: Wider
  -> Wider
  -> Wider
mul (Wider a) (Wider b) =
  let !(# l, _ #) = mul_c# a b
  in  Wider l

sqr#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# (# Word#, Word#, Word#, Word# #), (# Word#, Word#, Word#, Word# #) #)
sqr# (# x0, x1, x2, x3 #) =
  let !sh = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !(# q1_0, c1_0 #) = L.mac# x1 x0 0## 0##
      !r1 = c1_0
      !(# r2_0, c2_0 #) = L.mac# x2 x0 r1 0##
      !(# s2_1, c2_1 #) = L.mac# x2 x1 0## c2_0
      !t2 = c2_1
      !(# s3_0, c3_0 #) = L.mac# x3 x0 s2_1 0##
      !(# t3, c3_1 #) = L.mac# x3 x1 t2 c3_0
      !(# u3, c3_2 #) = L.mac# x3 x2 0## c3_1
      !v3 = c3_2
      !(# lo1, car0_1 #) =
        (# uncheckedShiftL# q1_0 1#, uncheckedShiftRL# q1_0 sh #)
      !(# lo2, car0_2 #) =
        (# or# (uncheckedShiftL# r2_0 1#) car0_1, uncheckedShiftRL# r2_0 sh #)
      !(# lo3, car0_3 #) =
        (# or# (uncheckedShiftL# s3_0 1#) car0_2, uncheckedShiftRL# s3_0 sh #)
      !(# hi0, car1_0 #) =
        (# or# (uncheckedShiftL# t3 1#) car0_3, uncheckedShiftRL# t3 sh #)
      !(# hi1, car1_1 #) =
        (# or# (uncheckedShiftL# u3 1#) car1_0, uncheckedShiftRL# u3 sh #)
      !(# hi2, car1_2 #) =
        (# or# (uncheckedShiftL# v3 1#) car1_1, uncheckedShiftRL# v3 sh #)
      !hi3 = car1_2
      !(# pf, car2_0 #) = L.mac# x0 x0 0## 0##
      !(# qf, car2_1 #) = L.add_c# lo1 car2_0 0##
      !(# rf, car2_2 #) = L.mac# x1 x1 lo2 car2_1
      !(# sf, car2_3 #) = L.add_c# lo3 car2_2 0##
      !(# tf, car2_4 #) = L.mac# x2 x2 hi0 car2_3
      !(# uf, car2_5 #) = L.add_c# hi1 car2_4 0##
      !(# vf, car2_6 #) = L.mac# x3 x3 hi2 car2_5
      !(# wf, _      #) = L.add_c# hi3 car2_6 0##
  in  (# (# pf, qf, rf, sf #), (# tf, uf, vf, wf #) #)
{-# INLINE sqr# #-}

sqr :: Wider -> (Wider, Wider)
sqr (Wider w) =
  let !(# l, h #) = sqr# w
  in  (Wider l, Wider h)

