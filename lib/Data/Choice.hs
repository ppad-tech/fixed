{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Choice (
  -- * Choice
    Choice
  , true#
  , false#
  , decide
  , to_word#

  -- * MaybeWord#
  , MaybeWord#(..)
  , some_word#
  , none_word#

  -- * MaybeWide#
  , MaybeWide#(..)
  , some_wide#
  , just_wide#
  , none_wide#
  , expect_wide#
  , expect_wide_or#

  -- * Construction
  , from_word_mask#
  , from_word_lsb#
  , from_word_nonzero#
  , from_word_eq#
  , from_word_le#
  , from_word_lt#
  , from_word_gt#

  , from_wide_lsb#
  , from_wide_le#

  -- * Manipulation
  , or_c#
  , and_c#
  , xor_c#
  , not_c#
  , ne_c#
  , eq_c#

  -- * Constant-time Selection
  , ct_select_word#
  , ct_select_wide#
  , ct_select_wider#

  -- * Constant-time Equality
  , ct_eq_word#
  , ct_eq_wide#
  , ct_eq_wider#
  ) where

import qualified Data.Bits as B
import GHC.Exts

-- utilities ------------------------------------------------------------------

-- make a mask from a bit (0 -> 0, 1 -> maxBound)
wrapping_neg# :: Word# -> Word#
wrapping_neg# w = plusWord# (not# w) 1##
{-# INLINE wrapping_neg# #-}

hi# :: Word# -> (# Word#, Word# #)
hi# w = (# 0##, w #)
{-# INLINE hi# #-}

lo# :: Word# -> (# Word#, Word# #)
lo# w = (# w, 0## #)
{-# INLINE lo# #-}

not_w# :: (# Word#, Word# #) -> (# Word#, Word# #)
not_w# (# a0, a1 #) = (# not# a0, not# a1 #)
{-# INLINE not_w# #-}

or_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
or_w# (# a0, a1 #) (# b0, b1 #) = (# or# a0 b0, or# a1 b1 #)
{-# INLINE or_w# #-}

and_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
and_w# (# a0, a1 #) (# b0, b1 #) = (# and# a0 b0, and# a1 b1 #)
{-# INLINE and_w# #-}

xor_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
xor_w# (# a0, a1 #) (# b0, b1 #) = (# xor# a0 b0, xor# a1 b1 #)
{-# INLINE xor_w# #-}

-- subtract-with-borrow
sub_b# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
sub_b# m n b =
  let !(# d0, b0 #) = subWordC# m n
      !(#  d, b1 #) = subWordC# d0 b
      !c = int2Word# (orI# b0 b1)
  in  (# d, c #)
{-# INLINE sub_b# #-}

-- wide subtract-with-borrow
sub_wb#
  :: (# Word#, Word# #)
  -> (# Word#, Word# #)
  -> (# Word#, Word#, Word# #)
sub_wb# (# a0, a1 #) (# b0, b1 #) =
  let !(# s0, c0 #) = sub_b# a0 b0 0##
      !(# s1, c1 #) = sub_b# a1 b1 c0
  in  (# s0, s1, c1 #)
{-# INLINE sub_wb# #-}

-- wide subtraction (wrapping)
sub_w# :: (# Word#, Word# #) -> (# Word#, Word# #) -> (# Word#, Word# #)
sub_w# a b =
  let !(# c0, c1, _ #) = sub_wb# a b
  in  (# c0, c1 #)
{-# INLINE sub_w# #-}

-- choice ---------------------------------------------------------------------

-- constant-time choice, encoded as a mask
newtype Choice = Choice Word#

false# :: () -> Choice
false# _ = Choice 0##
{-# INLINE false# #-}

true# :: () -> Choice
true# _ = case maxBound :: Word of
  W# w -> Choice w
{-# INLINE true# #-}

decide :: Choice -> Bool
decide (Choice c) = isTrue# (neWord# c 0##)
{-# INLINE decide #-}

to_word# :: Choice -> Word#
to_word# (Choice c) = and# c 1##
{-# INLINE to_word# #-}

-- constant time 'Maybe Word#'
newtype MaybeWord# = MaybeWord# (# Word#, Choice #)

some_word# :: Word# -> MaybeWord#
some_word# w = MaybeWord# (# w, true# () #)
{-# INLINE some_word# #-}

none_word# :: Word# -> MaybeWord#
none_word# w = MaybeWord# (# w, false# () #)
{-# INLINE none_word# #-}

-- constant time 'Maybe (# Word#, Word# #)'
newtype MaybeWide# = MaybeWide# (# (# Word#, Word# #), Choice #)

just_wide# :: (# Word#, Word# #) -> Choice -> MaybeWide#
just_wide# w c = MaybeWide# (# w, c #)
{-# INLINE just_wide# #-}

some_wide# :: (# Word#, Word# #) -> MaybeWide#
some_wide# w = MaybeWide# (# w, true# () #)
{-# INLINE some_wide# #-}

none_wide# :: (# Word#, Word# #) -> MaybeWide#
none_wide# w = MaybeWide# (# w, false# () #)
{-# INLINE none_wide# #-}

expect_wide# :: MaybeWide# -> String -> (# Word#, Word# #)
expect_wide# (MaybeWide# (# w, Choice c #)) msg
    | isTrue# (eqWord# c t#) = w
    | otherwise = error $ "ppad-fixed (expect_wide#): " <> msg
  where
    !(Choice t#) = true# ()
{-# INLINE expect_wide# #-}

expect_wide_or# :: MaybeWide# -> (# Word#, Word# #) -> (# Word#, Word# #)
expect_wide_or# (MaybeWide# (# w, Choice c #)) alt
    | isTrue# (eqWord# c t#) = w
    | otherwise = alt
  where
    !(Choice t#) = true# ()
{-# INLINE expect_wide_or# #-}

-- construction ---------------------------------------------------------------

from_word_mask# :: Word# -> Choice
from_word_mask# w = Choice w
{-# INLINE from_word_mask# #-}

from_word_lsb# :: Word# -> Choice
from_word_lsb# w = Choice (wrapping_neg# w)
{-# INLINE from_word_lsb# #-}

from_wide_lsb# :: (# Word#, Word# #) -> Choice
from_wide_lsb# (# l, _ #) = from_word_lsb# l
{-# INLINE from_wide_lsb# #-}

from_word_nonzero# :: Word# -> Choice
from_word_nonzero# w =
  let !n = wrapping_neg# w
      !s = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !v = uncheckedShiftRL# (or# w n) s
  in  from_word_lsb# v
{-# INLINE from_word_nonzero# #-}

from_word_eq# :: Word# -> Word# -> Choice
from_word_eq# x y = case from_word_nonzero# (xor# x y) of
  Choice w -> Choice (not# w)
{-# INLINE from_word_eq# #-}

from_word_le# :: Word# -> Word# -> Choice
from_word_le# x y =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !bit =
        uncheckedShiftRL#
          (and#
            (or# (not# x) y)
            (or# (xor# x y) (not# (minusWord# y x))))
          s
  in  from_word_lsb# bit
{-# INLINE from_word_le# #-}

from_wide_le# :: (# Word#, Word# #) -> (# Word#, Word# #) -> Choice
from_wide_le# x y =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !mask =
        (and_w#
          (or_w# (not_w# x) y)
          (or_w# (xor_w# x y) (not_w# (sub_w# y x))))
      !bit = case mask of
        (# l, _ #) -> uncheckedShiftRL# l s
  in  from_word_lsb# bit
{-# INLINE from_wide_le# #-}

from_word_lt# :: Word# -> Word# -> Choice
from_word_lt# x y =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !bit =
        uncheckedShiftRL#
          (or#
            (and# (not# x) y)
            (and# (or# (not# x) y) (minusWord# x y)))
          s
  in  from_word_lsb# bit
{-# INLINE from_word_lt# #-}

from_word_gt# :: Word# -> Word# -> Choice
from_word_gt# x y = from_word_lt# y x
{-# INLINE from_word_gt# #-}

-- manipulation ---------------------------------------------------------------

not_c# :: Choice -> Choice
not_c# (Choice w) = Choice (not# w)
{-# INLINE not_c# #-}

or_c# :: Choice -> Choice -> Choice
or_c# (Choice w0) (Choice w1) = Choice (or# w0 w1)
{-# INLINE or_c# #-}

and_c# :: Choice -> Choice -> Choice
and_c# (Choice w0) (Choice w1) = Choice (and# w0 w1)
{-# INLINE and_c# #-}

xor_c# :: Choice -> Choice -> Choice
xor_c# (Choice w0) (Choice w1) = Choice (xor# w0 w1)
{-# INLINE xor_c# #-}

ne_c# :: Choice -> Choice -> Choice
ne_c# c0 c1 = xor_c# c0 c1
{-# INLINE ne_c# #-}

eq_c# :: Choice -> Choice -> Choice
eq_c# c0 c1 = not_c# (ne_c# c0 c1)
{-# INLINE eq_c# #-}

-- constant-time selection ----------------------------------------------------

ct_select_word# :: Word# -> Word# -> Choice -> Word#
ct_select_word# a b (Choice c) = xor# a (and# c (xor# a b))
{-# INLINE ct_select_word# #-}

ct_select_wide#
  :: (# Word#, Word# #)
  -> (# Word#, Word# #)
  -> Choice
  -> (# Word#, Word# #)
ct_select_wide# a b (Choice w) =
  let !mask = or_w# (hi# w) (lo# w)
  in  xor_w# a (and_w# mask (xor_w# a b))
{-# INLINE ct_select_wide# #-}

ct_select_wider#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> Choice
  -> (# Word#, Word#, Word#, Word# #)
ct_select_wider# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) (Choice w) =
  let !w0 = xor# a0 (and# w (xor# a0 b0))
      !w1 = xor# a1 (and# w (xor# a1 b1))
      !w2 = xor# a2 (and# w (xor# a2 b2))
      !w3 = xor# a3 (and# w (xor# a3 b3))
  in  (# w0, w1, w2, w3 #)
{-# INLINE ct_select_wider# #-}

-- constant-time equality -----------------------------------------------------

ct_eq_word# :: Word# -> Word# -> Choice
ct_eq_word# a b =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !x = xor# a b
      !y = uncheckedShiftRL# (or# x (wrapping_neg# x)) s
  in  Choice (xor# y 1##)
{-# INLINE ct_eq_word# #-}

ct_eq_wide#
  :: (# Word#, Word# #)
  -> (# Word#, Word# #)
  -> Choice
ct_eq_wide# (# a0, a1 #) (# b0, b1 #) =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !x = or# (xor# a0 b0) (xor# a1 b1)
      !y = uncheckedShiftRL# (or# x (wrapping_neg# x)) s
  in  Choice (xor# y 1##)
{-# INLINE ct_eq_wide# #-}

ct_eq_wider#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> Choice
ct_eq_wider# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #) =
  let !s = case B.finiteBitSize (0 :: Word) of I# m -> m -# 1#
      !x = or# (or# (xor# a0 b0) (xor# a1 b1))
               (or# (xor# a2 b2) (xor# a3 b3))
      !y = uncheckedShiftRL# (or# x (wrapping_neg# x)) s
  in  Choice (xor# y 1##)
{-# INLINE ct_eq_wider# #-}

