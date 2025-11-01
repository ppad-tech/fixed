{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Word.Wider where

import Control.DeepSeq
import qualified Data.Choice as C
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import qualified Data.Bits as B
import qualified Data.Word.Limb as L
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

wrapping_neg# :: Word# -> Word#
wrapping_neg# w = plusWord# (not# w) 1##
{-# INLINE wrapping_neg# #-}

-- wide words -----------------------------------------------------------------

-- little-endian, i.e. (# w0, w1, w2, w3 #)
data Wider = Wider (# Word#, Word#, Word#, Word# #)

instance Show Wider where
  show (Wider (# a, b, c, d #)) =
       "(" <> show (W# a) <> ", " <> show (W# b) <> ", "
    <> show (W# c) <> ", " <> show (W# d) <> ")"

instance Eq Wider where
  Wider (# a0, b0, c0, d0 #) == Wider (# a1, b1, c1, d1 #) =
    isTrue# (andI#
      ((andI# (eqWord# a0 a1) (eqWord# b0 b1)))
      ((andI# (eqWord# c0 c1) (eqWord# d0 d1))))

instance NFData Wider where
  rnf (Wider a) = case a of (# _, _, _, _ #) -> ()

-- construction / conversion --------------------------------------------------

-- construct from lo, hi
wider :: Word -> Word -> Word -> Word -> Wider
wider (W# w0) (W# w1) (W# w2) (W# w3) = Wider (# w0, w1, w2, w3 #)

to :: Integer -> Wider
to n =
  let !size = B.finiteBitSize (0 :: Word)
      !mask = fi (maxBound :: Word) :: Integer
      !(W# w0) = fi (n .&. mask)
      !(W# w1) = fi ((n .>>. size) .&. mask)
      !(W# w2) = fi ((n .>>. (2 * size)) .&. mask)
      !(W# w3) = fi ((n .>>. (3 * size)) .&. mask)
  in  Wider (# w0, w1, w2, w3 #)

from :: Wider -> Integer
from (Wider (# w0, w1, w2, w3 #)) =
        fi (W# w3) .<<. (3 * size)
    .|. fi (W# w2) .<<. (2 * size)
    .|. fi (W# w1) .<<. size
    .|. fi (W# w0)
  where
    !size = B.finiteBitSize (0 :: Word)

-- wider-add-with-carry, i.e. (# sum, carry bit #)
add_wc#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word#, Word# #)
add_wc# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  let !(# s0, c0 #) = L.add_c# a0 b0 0##
      !(# s1, c1 #) = L.add_c# a1 b1 c0
      !(# s2, c2 #) = L.add_c# a2 b2 c1
      !(# s3, c3 #) = L.add_c# a3 b3 c2
  in  (# s0, s1, s2, s3, c3 #)
{-# INLINE add_wc# #-}

-- wider addition (wrapping)
add_w#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
add_w# a b =
  let !(# c0, c1, c2, c3, _ #) = add_wc# a b
  in  (# c0, c1, c2, c3 #)
{-# INLINE add_w# #-}

-- reference: borrowing_sub
sub_b#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word#, Word# #) -- (# difference, borrow bit #)
sub_b# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  let !(# s0, c0 #) = L.sub_b# a0 b0 0##
      !(# s1, c1 #) = L.sub_b# a1 b1 c0
      !(# s2, c2 #) = L.sub_b# a2 b2 c1
      !(# s3, c3 #) = L.sub_b# a3 b3 c2
  in  (# s0, s1, s2, s3, c3 #)
{-# INLINE sub_b# #-}

-- reference sub_mod_with_carry
sub_mod_c#
  :: (# Word#, Word#, Word#, Word# #) -- lhs
  -> Word#                            -- carry bit
  -> (# Word#, Word#, Word#, Word# #) -- rhs
  -> (# Word#, Word#, Word#, Word# #) -- p
  -> (# Word#, Word#, Word#, Word# #)
sub_mod_c# a c b (# p0, p1, p2, p3 #) =
  let !(# o0, o1, o2, o3, bb #) = sub_b# a b
      !mask = and# (not# (wrapping_neg# c)) (wrapping_neg# bb)
      !band = (# and# p0 mask, and# p1 mask, and# p2 mask, and# p3 mask #)
  in  add_w# (# o0, o1, o2, o3 #) band
{-# INLINE sub_mod_c# #-}

