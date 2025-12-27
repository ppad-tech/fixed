{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.Choice
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Primitives for constant-time choice.
--
-- The 'Choice' type encodes truthy and falsy values as unboxed 'Word#'
-- bit masks. You should convert it to a 'Bool' using 'decide' at the /end/
-- of a computation that you want to run in constant time.
--
-- Use the standard logical primitives ('or', 'and', 'xor', 'not', eq') to
-- manipulate "in-flight" 'Choice' values.

module Data.Choice (
  -- * Choice
    Choice
  , true#
  , false#
  , decide
  , to_word#

  -- * Construction
  , from_full_mask#
  , from_bit#
  , from_word_nonzero#
  , from_word_eq#
  , from_word_le#
  , from_word_lt#
  , from_word_gt#

  -- * Manipulation
  , or
  , and
  , xor
  , not
  , ne
  , eq

  -- * Constant-time Selection
  , select_word#
  , select_wide#
  , select_wider#

  -- * Constant-time Equality
  , eq_word#
  , eq_wide#
  , eq_wider#
  ) where

import qualified Data.Bits as B
import GHC.Exts (Word#, Int(..), Word(..))
import qualified GHC.Exts as Exts
import Prelude hiding (and, not, or)

-- utilities ------------------------------------------------------------------

-- wrapping negation
neg_w# :: Word# -> Word#
neg_w# w = Exts.plusWord# (Exts.not# w) 1##
{-# INLINE neg_w# #-}

hi# :: Word# -> (# Word#, Word# #)
hi# w = (# 0##, w #)
{-# INLINE hi# #-}

lo# :: Word# -> (# Word#, Word# #)
lo# w = (# w, 0## #)
{-# INLINE lo# #-}

or_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
or_w# (# a0, a1 #) (# b0, b1 #) = (# Exts.or# a0 b0, Exts.or# a1 b1 #)
{-# INLINE or_w# #-}

and_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
and_w# (# a0, a1 #) (# b0, b1 #) = (# Exts.and# a0 b0, Exts.and# a1 b1 #)
{-# INLINE and_w# #-}

xor_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
xor_w# (# a0, a1 #) (# b0, b1 #) = (# Exts.xor# a0 b0, Exts.xor# a1 b1 #)
{-# INLINE xor_w# #-}

-- choice ---------------------------------------------------------------------

-- | Constant-time choice, encoded as a mask.
--
--   Note that 'Choice' is defined as an unboxed newtype, and so a
--   'Choice' value cannot be bound at the top level. You should work
--   with it locally in the context of a computation.
--
--   >>> decide (or# (false# ()) (true# ()))
--   True
newtype Choice = Choice Word#

-- | Construct the falsy 'Choice'.
--
--   >>> decide (false# ())
--   False
false# :: () -> Choice
false# _ = Choice 0##
{-# INLINE false# #-}

-- | Construct the truthy 'Choice'.
--
--   >>> decide (true# ())
--   True
true# :: () -> Choice
true# _ = case maxBound :: Word of
  W# w -> Choice w
{-# INLINE true# #-}

-- | Decide a 'Choice' by reducing it to a 'Bool'.
--
--   The 'decide' function itself runs in constant time, but once
--   it reduces a 'Choice' to a 'Bool', any subsequent branching on
--   the result is liable to introduce variable-time behaviour.
--
--   You should 'decide' only at the /end/ of a computation, after all
--   security-sensitive computations have been carried out.
--
--   >>> decide (true# ())
--   True
decide :: Choice -> Bool
decide (Choice c) = Exts.isTrue# (Exts.neWord# c 0##)
{-# INLINE decide #-}

-- | Convert a 'Choice' to an unboxed 'Word#'.
--
--   This essentially "unboxes" the 'Choice' for direct manipulation.
--
--   >>> import qualified GHC.Exts as Exts
--   >>> Exts.isTrue# (Exts.eqWord# 0## (to_word# (false# ())))
--   True
to_word# :: Choice -> Word#
to_word# (Choice c) = Exts.and# c 1##
{-# INLINE to_word# #-}

-- construction ---------------------------------------------------------------

-- | Construct a 'Choice' from an unboxed full-word mask.
--
--   The input is /not/ checked to be a full-word mask.
--
--   >>> decide (from_full_mask# 0##)
--   False
--   >>> decide (from_full_mask# 0xFFFFFFFFF_FFFFFFFF##)
--   True
from_full_mask# :: Word# -> Choice
from_full_mask# w = Choice w
{-# INLINE from_full_mask# #-}

-- | Construct a 'Choice' from an unboxed word, which should be either
--   0## or 1##.
--
--   The input is /not/ checked to be a bit.
--
--   >>> decide (from_bit# 1##)
--   True
from_bit# :: Word# -> Choice
from_bit# w = Choice (neg_w# w)
{-# INLINE from_bit# #-}

-- | Construct a 'Choice' from a /nonzero/ unboxed word.
--
--   The input is /not/ checked to be nonzero.
--
--   >>> decide (from_word_nonzero# 2##)
--   True
from_word_nonzero# :: Word# -> Choice
from_word_nonzero# w =
  let !n = neg_w# w
      !s = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !v = Exts.uncheckedShiftRL# (Exts.or# w n) s
  in  from_bit# v
{-# INLINE from_word_nonzero# #-}

-- | Construct a 'Choice' from an equality comparison.
--
--   >>> decide (from_word_eq# 0## 1##)
--   False
--   decide (from_word_eq# 1## 1##)
--   True
from_word_eq# :: Word# -> Word# -> Choice
from_word_eq# x y = case from_word_nonzero# (Exts.xor# x y) of
  Choice w -> Choice (Exts.not# w)
{-# INLINE from_word_eq# #-}

-- | Construct a 'Choice from an at-most comparison.
--
--   >>> decide (from_word_le# 0## 1##)
--   True
--   >>> decide (from_word_le# 1## 1##)
--   True
from_word_le# :: Word# -> Word# -> Choice
from_word_le# x y =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !bit =
        Exts.uncheckedShiftRL#
          (Exts.and#
            (Exts.or# (Exts.not# x) y)
            (Exts.or# (Exts.xor# x y) (Exts.not# (Exts.minusWord# y x))))
          s
  in  from_bit# bit
{-# INLINE from_word_le# #-}

-- | Construct a 'Choice' from a less-than comparison.
--
--   >>> decide (from_word_lt# 0## 1##)
--   True
--   >>> decide (from_word_lt# 1## 1##)
--   False
from_word_lt# :: Word# -> Word# -> Choice
from_word_lt# x y =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !bit =
        Exts.uncheckedShiftRL#
          (Exts.or#
            (Exts.and# (Exts.not# x) y)
            (Exts.and# (Exts.or# (Exts.not# x) y) (Exts.minusWord# x y)))
          s
  in  from_bit# bit
{-# INLINE from_word_lt# #-}

-- | Construct a 'Choice' from a greater-than comparison.
--
--   >>> decide (from_word_gt# 0## 1##)
--   False
--   >>> decide (from_word_gt# 1## 1##)
--   False
from_word_gt# :: Word# -> Word# -> Choice
from_word_gt# x y = from_word_lt# y x
{-# INLINE from_word_gt# #-}

-- manipulation ---------------------------------------------------------------

-- | Logically negate a 'Choice'.
--
--   >>> decide (not (true# ()))
--   False
--   >>> decide (not (false# ()))
--   True
not :: Choice -> Choice
not (Choice w) = Choice (Exts.not# w)
{-# INLINE not #-}

-- | Logical disjunction on 'Choice' values.
--
--   >>> decide (or (true# ()) (false# ()))
--   True
or :: Choice -> Choice -> Choice
or (Choice w0) (Choice w1) = Choice (Exts.or# w0 w1)
{-# INLINE or #-}

-- | Logical conjunction on 'Choice' values.
--
--   >>> decide (and (true# ()) (false# ()))
--   False
and :: Choice -> Choice -> Choice
and (Choice w0) (Choice w1) = Choice (Exts.and# w0 w1)
{-# INLINE and #-}

-- | Logical inequality on 'Choice' values.
--
--   >>> decide (xor (true# ()) (false# ()))
--   True
xor :: Choice -> Choice -> Choice
xor (Choice w0) (Choice w1) = Choice (Exts.xor# w0 w1)
{-# INLINE xor #-}

-- | Logical inequality on 'Choice' values.
--
--   >>> decide (ne (true# ()) (false# ()))
--   True
ne :: Choice -> Choice -> Choice
ne c0 c1 = xor c0 c1
{-# INLINE ne #-}

-- | Logical equality on 'Choice' values.
--
--   >>> decide (eq (true# ()) (false# ()))
--   False
eq :: Choice -> Choice -> Choice
eq c0 c1 = not (ne c0 c1)
{-# INLINE eq #-}

-- constant-time selection ----------------------------------------------------

-- | Select an unboxed word, given a 'Choice'.
--
--   >>> let w = C.select_word# 0## 1## (C.true# ()) in GHC.Word.W# w
--   1
select_word# :: Word# -> Word# -> Choice -> Word#
select_word# a b (Choice c) = Exts.xor# a (Exts.and# c (Exts.xor# a b))
{-# INLINE select_word# #-}

-- | Select an unboxed two-limb word, given a 'Choice'.
select_wide#
  :: (# Word#, Word# #)
  -> (# Word#, Word# #)
  -> Choice
  -> (# Word#, Word# #)
select_wide# a b (Choice w) =
  let !mask = or_w# (hi# w) (lo# w)
  in  xor_w# a (and_w# mask (xor_w# a b))
{-# INLINE select_wide# #-}

-- | Select an unboxed four-limb word, given a 'Choice'.
select_wider#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> Choice
  -> (# Word#, Word#, Word#, Word# #)
select_wider# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) (Choice w) =
  let !w0 = Exts.xor# a0 (Exts.and# w (Exts.xor# a0 b0))
      !w1 = Exts.xor# a1 (Exts.and# w (Exts.xor# a1 b1))
      !w2 = Exts.xor# a2 (Exts.and# w (Exts.xor# a2 b2))
      !w3 = Exts.xor# a3 (Exts.and# w (Exts.xor# a3 b3))
  in  (# w0, w1, w2, w3 #)
{-# INLINE select_wider# #-}

-- constant-time equality -----------------------------------------------------

-- | Compare unboxed words for equality in constant time.
--
--   >>> decide (eq_word# 0## 1##)
--   False
eq_word# :: Word# -> Word# -> Choice
eq_word# a b =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !x = Exts.xor# a b
      !y = Exts.uncheckedShiftRL# (Exts.or# x (neg_w# x)) s
  in  Choice (Exts.xor# y 1##)
{-# INLINE eq_word# #-}

-- | Compare unboxed two-limb words for equality in constant time.
--
--   >>> decide (eq_wide (# 0##, 0## #) (# 0##, 0## #))
--   True
eq_wide#
  :: (# Word#, Word# #)
  -> (# Word#, Word# #)
  -> Choice
eq_wide# (# a0, a1 #) (# b0, b1 #) =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !x = Exts.or# (Exts.xor# a0 b0) (Exts.xor# a1 b1)
      !y = Exts.uncheckedShiftRL# (Exts.or# x (neg_w# x)) s
  in  Choice (Exts.xor# y 1##)
{-# INLINE eq_wide# #-}

-- | Compare unboxed four-limb words for equality in constant time.
--
--   >>> let zero = (# 0##, 0##, 0##, 0## #) in decide (eq_wider# zero zero)
--   True
eq_wider#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> Choice
eq_wider# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m Exts.-# 1#
      !x = Exts.or# (Exts.or# (Exts.xor# a0 b0) (Exts.xor# a1 b1))
                    (Exts.or# (Exts.xor# a2 b2) (Exts.xor# a3 b3))
      !y = Exts.uncheckedShiftRL# (Exts.or# x (neg_w# x)) s
  in  Choice (Exts.xor# y 1##)
{-# INLINE eq_wider# #-}

