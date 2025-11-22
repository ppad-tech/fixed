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

instance Show Wider where
  show (Wider (# a, b, c, d #)) =
       "(" <> show (W# a) <> ", " <> show (W# b) <> ", "
    <> show (W# c) <> ", " <> show (W# d) <> ")"

instance NFData Wider where
  rnf (Wider a) = case a of (# _, _, _, _ #) -> ()

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
