{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Numeric.Montgomery.Secp256k1.Curve where

import Control.DeepSeq
import qualified Data.Choice as C
import qualified Data.Word.Limb as L
import qualified Data.Word.Wide as W
import Data.Word.Wider (Wider(..))
import qualified Data.Word.Wider as WW
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- montgomery arithmetic, specialized to the secp256k1 field prime modulus
-- 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

data Montgomery = Montgomery !(# Word#, Word#, Word#, Word# #)

instance Show Montgomery where
  show (Montgomery (# a, b, c, d #)) =
       "(" <> show (W# a) <> ", " <> show (W# b) <> ", "
    <> show (W# c) <> ", " <> show (W# d) <> ")"

instance Eq Montgomery where
  Montgomery a == Montgomery b = C.decide (C.ct_eq_wider# a b)

instance NFData Montgomery where
  rnf (Montgomery a) = case a of (# _, _, _, _ #) -> ()

instance Num Montgomery where
  a + b = add a b
  a - b = sub a b
  a * b = mul a b
  negate a = neg a
  abs = id
  signum a = case a of
    Montgomery (# 0##, 0##, 0##, 0## #) -> 0
    _ -> 1
  fromInteger = to . WW.to

-- XX define constants here, current approach is fragile

-- utilities ------------------------------------------------------------------

-- Wide wrapping addition, when addend is only a limb.
wadd_w# :: (# Word#, Word# #) -> Word# -> (# Word#, Word# #)
wadd_w# (# x_lo, x_hi #) y_lo =
  let !(# c0, s0 #) = plusWord2# x_lo y_lo
      !(# _, s1 #) = plusWord2# x_hi c0
  in  (# s0, s1 #)
{-# INLINE wadd_w# #-}

-- Truncate an unboxed wide word to an unboxed word.
lo :: (# Word#, Word# #) -> Word#
lo (# l, _ #) = l
{-# INLINE lo #-}

-- innards --------------------------------------------------------------------

redc_inner#
  :: (# Word#, Word#, Word#, Word# #)              -- ^ upper words
  -> (# Word#, Word#, Word#, Word# #)              -- ^ lower words
  -> (# (# Word#, Word#, Word#, Word# #), Word# #) -- ^ upper words, meta-carry
redc_inner# (# u0, u1, u2, u3 #) (# l0, l1, l2, l3 #) =
  let -- secp256k1 constants
      !(# m0, m1, m2, m3 #) =
        (# 0xFFFFFFFEFFFFFC2F##, 0xFFFFFFFFFFFFFFFF##
        ,  0xFFFFFFFFFFFFFFFF##, 0xFFFFFFFFFFFFFFFF## #)
      !n = 0xD838091DD2253531##
      -- outer loop, i == 0 ---------------------------------------------------
      !w_0 = L.mul_w# l0 n
      !(# _, c_00 #) = L.mac# w_0 m0 l0 0##     -- m0, l0
      -- first inner loop (j < 4)
      !(# l0_1, c_01 #) = L.mac# w_0 m1 l1 c_00 -- l<i idx>_<j idx>
      !(# l0_2, c_02 #) = L.mac# w_0 m2 l2 c_01
      !(# l0_3, c_03 #) = L.mac# w_0 m3 l3 c_02
      -- final stanza
      !(# u_0, mc_0 #) = L.add_c# u0 c_03 0##
      -- end states
      -- (# l0, l0_1, l0_2, l0_3 #)
      -- (# u_0, u1, u2, u3 #)
      -- outer loop, i == 1 ---------------------------------------------------
      !w_1 = L.mul_w# l0_1 n
      !(# _, c_10 #) = L.mac# w_1 m0 l0_1 0##
      -- first inner loop (j < 3)
      !(# l1_1, c_11 #) = L.mac# w_1 m1 l0_2 c_10 -- j == 1
      !(# l1_2, c_12 #) = L.mac# w_1 m2 l0_3 c_11 -- j == 2
      -- second inner loop (j < 4)
      !(# u1_3, c_13 #) = L.mac# w_1 m3 u_0 c_12  -- j == 3
      -- final stanza
      !(# u_1, mc_1 #) = L.add_c# u1 c_13 mc_0
      -- end states
      -- (# l0, l0_1, l1_1, l1_2 #)
      -- (# u1_3, u_1, u2, u3 #)
      -- outer loop, i == 2 ---------------------------------------------------
      !w_2 = L.mul_w# l1_1 n
      !(# _, c_20 #) = L.mac# w_2 m0 l1_1 0##
      -- first inner loop (j < 2)
      !(# l2_1, c_21 #) = L.mac# w_2 m1 l1_2 c_20  -- j == 1
      -- second inner loop (j < 4)
      !(# u2_2, c_22 #) = L.mac# w_2 m2 u1_3 c_21  -- j == 2
      !(# u2_3, c_23 #) = L.mac# w_2 m3 u_1 c_22   -- j == 3
      -- final stanza
      !(# u_2, mc_2 #) = L.add_c# u2 c_23 mc_1
      -- end states
      -- (# l0, l0_1, l1_1, l2_1 #)
      -- (# u2_2, u2_3, u_2, u3 #)
      -- outer loop, i == 3 ---------------------------------------------------
      !w_3 = L.mul_w# l2_1 n
      !(# _, c_30 #) = L.mac# w_3 m0 l2_1 0##
      -- second inner loop (j < 4)
      !(# u3_1, c_31 #) = L.mac# w_3 m1 u2_2 c_30  -- j == 1
      !(# u3_2, c_32 #) = L.mac# w_3 m2 u2_3 c_31  -- j == 2
      !(# u3_3, c_33 #) = L.mac# w_3 m3 u_2 c_32   -- j == 3
      -- final stanza
      !(# u_3, mc_3 #) = L.add_c# u3 c_33 mc_2
      -- end states
      -- (# l0, l0_1, l1_1, l2_1 #)
      -- (# u3_1, u3_2, u3_3, u_3 #)
  in  (# (# u3_1, u3_2, u3_3, u_3 #), mc_3 #)
{-# INLINE redc_inner# #-}

-- | Montgomery reduction.
redc#
  :: (# Word#, Word#, Word#, Word# #) -- ^ lower words
  -> (# Word#, Word#, Word#, Word# #) -- ^ upper words
  -> (# Word#, Word#, Word#, Word# #) -- ^ result
redc# l u =
  let !m = (# 0xFFFFFFFEFFFFFC2F##, 0xFFFFFFFFFFFFFFFF##    -- field prime
           ,  0xFFFFFFFFFFFFFFFF##, 0xFFFFFFFFFFFFFFFF## #)
      !(# nu, mc #) = redc_inner# u l
  in  WW.sub_mod_c# nu mc m m
{-# INLINE redc# #-}

redc :: Montgomery -> Montgomery -> Montgomery
redc (Montgomery l) (Montgomery u) =
  let !res = redc# l u
  in  (Montgomery res)

retr_inner#
  :: (# Word#, Word#, Word#, Word# #) -- ^ value in montgomery form
  -> (# Word#, Word#, Word#, Word# #) -- ^ retrieved value
retr_inner# (# x0, x1, x2, x3 #) =
  let -- secp256k1 constants
      !(# m0, m1, m2, m3 #) =
        (# 0xFFFFFFFEFFFFFC2F##, 0xFFFFFFFFFFFFFFFF##
        ,  0xFFFFFFFFFFFFFFFF##, 0xFFFFFFFFFFFFFFFF## #)
      !n = 0xD838091DD2253531##
      -- outer loop, i == 0 ---------------------------------------------------
      !u_0 = L.mul_w# x0 n                            -- out state
      !(# _, o0 #) = L.mac# u_0 m0 x0 0##             -- o0, 0, 0, 0
      -- inner loop
      !(# o0_1, p0_1 #) = L.mac# u_0 m1 0## o0        -- o0_1, p0_1, 0, 0
      !(# p0_2, q0_2 #) = L.mac# u_0 m2 0## p0_1      -- o0_1, p0_2, q0_2, 0
      !(# q0_3, r0_3 #) = L.mac# u_0 m3 0## q0_2      -- o0_1, p0_2, q0_3, r0_3
      -- end state: (# o0_1, p0_2, q0_3, r0_3 #)
      -- outer loop, i == 1 ---------------------------------------------------
      !u_1 = L.mul_w# (plusWord# o0_1 x1) n
      !(# _, o1 #) = L.mac# u_1 m0 x1 o0_1            -- o1, p0_2, q0_3, r0_3
      -- inner loop
      !(# o1_1, p1_1 #) = L.mac# u_1 m1 p0_2 o1       -- o1_1, p1_1, q0_3, r0_3
      !(# p1_2, q1_2 #) = L.mac# u_1 m2 q0_3 p1_1     -- o1_1, p1_2, q1_2, r0_3
      !(# q1_3, r1_3 #) = L.mac# u_1 m3 r0_3 q1_2     -- o1_1, p1_2, q1_3, r1_3
      -- end state: (# o1_1, p1_2, q1_3, r1_3 #)
      -- outer loop, i == 2 ---------------------------------------------------
      !u_2 = L.mul_w# (plusWord# o1_1 x2) n
      !(# _, o2 #) = L.mac# u_2 m0 x2 o1_1            -- o2, p1_2, q1_3, r1_3
      -- inner loop
      !(# o2_1, p2_1 #) = L.mac# u_2 m1 p1_2 o2       -- o2_1, p2_1, q1_3, r1_3
      !(# p2_2, q2_2 #) = L.mac# u_2 m2 q1_3 p2_1     -- o2_1, p2_2, q2_2, r1_3
      !(# q2_3, r2_3 #) = L.mac# u_2 m3 r1_3 q2_2     -- o2_1, p2_2, q2_3, r2_3
      -- end state: (# o2_1, p2_2, q2_3, r2_3 #)
      -- outer loop, i == 3 ---------------------------------------------------
      !u_3 = L.mul_w# (plusWord# o2_1 x3) n
      !(# _, o3 #) = L.mac# u_3 m0 x3 o2_1            -- o3, p2_2, q2_3, r2_3
      -- inner loop
      !(# o3_1, p3_1 #) = L.mac# u_3 m1 p2_2 o3       -- o3_1, p3_1, q2_3, r2_3
      !(# p3_2, q3_2 #) = L.mac# u_3 m2 q2_3 p3_1     -- o3_1, p3_2, q3_2, r2_3
      !(# q3_3, r3_3 #) = L.mac# u_3 m3 r2_3 q3_2     -- o3_1, p3_2, q3_3, r3_3
      -- final state: (# o3_1, p3_2, q3_3, r3_3 #)
  in  (# o3_1, p3_2, q3_3, r3_3 #)
{-# INLINE retr_inner# #-}

retr#
  :: (# Word#, Word#, Word#, Word# #) -- montgomery form
  -> (# Word#, Word#, Word#, Word# #)
retr# f = retr_inner# f
{-# INLINE retr# #-}

retr
  :: Montgomery -- ^ value in montgomery form
  -> Wider      -- ^ retrieved value
retr (Montgomery f) =
  let !res = retr# f
  in  (Wider res)

-- NB massive register pressure

-- | Montgomery multiplication (FIOS), without conditional subtract.
mul_inner#
  :: (# Word#, Word#, Word#, Word# #)              -- ^ x
  -> (# Word#, Word#, Word#, Word# #)              -- ^ y
  -> (# (# Word#, Word#, Word#, Word# #), Word# #) -- ^ product, meta-carry
mul_inner# (# x0, x1, x2, x3 #) (# y0, y1, y2, y3 #) =
  let -- secp256k1 constants
      !(# m0, m1, m2, m3 #) =
        (# 0xFFFFFFFEFFFFFC2F##, 0xFFFFFFFFFFFFFFFF##
        ,  0xFFFFFFFFFFFFFFFF##, 0xFFFFFFFFFFFFFFFF## #)
      !n = 0xD838091DD2253531##
      -- outer loop, i == 0 ---------------------------------------------------
      !axy0 = L.mul_c# x0 y0                                  -- out state
      !u0 = L.mul_w# (lo axy0) n                              -- 0, 0, 0, 0
      !(# (# _, a0 #), c0 #) = W.add_c# (L.mul_c# u0 m0) axy0
      !carry0 = (# a0, c0 #)
      -- inner loop, j == 1
      !axy0_1 = L.mul_c# x0 y1
      !umc0_1 = W.add_w# (L.mul_c# u0 m1) carry0
      !(# (# o0, ab0_1 #), c0_1 #) = W.add_c# axy0_1 umc0_1   -- o0, 0, 0, 0
      !carry0_1 = (# ab0_1, c0_1 #)
      -- inner loop, j == 2
      !axy0_2 = L.mul_c# x0 y2
      !umc0_2 = W.add_w# (L.mul_c# u0 m2) carry0_1
      !(# (# p0, ab0_2 #), c0_2 #) = W.add_c# axy0_2 umc0_2   -- o0, p0, 0, 0
      !carry0_2 = (# ab0_2, c0_2 #)
      -- inner loop, j == 3
      !axy0_3 = L.mul_c# x0 y3
      !umc0_3 = W.add_w# (L.mul_c# u0 m3) carry0_2
      !(# (# q0, ab0_3 #), c0_3 #) = W.add_c# axy0_3 umc0_3   -- o0, p0, q0, 0
      !carry0_3 = (# ab0_3, c0_3 #)
      -- final stanza
      !(# r0, mc0 #) = carry0_3                               -- o0, p0, q0, r0
      -- outer loop, i == 1 ---------------------------------------------------
      !axy1 = wadd_w# (L.mul_c# x1 y0) o0
      !u1 = L.mul_w# (lo axy1) n
      !(# (# _, a1 #), c1 #) = W.add_c# (L.mul_c# u1 m0) axy1
      !carry1 = (# a1, c1 #)
      -- inner loop, j == 1
      !axy1_1 = wadd_w# (L.mul_c# x1 y1) p0
      !umc1_1 = W.add_w# (L.mul_c# u1 m1) carry1
      !(# (# o1, ab1_1 #), c1_1 #) = W.add_c# axy1_1 umc1_1   -- o1, p0, q0, r0
      !carry1_1 = (# ab1_1, c1_1 #)
      -- inner loop, j == 2
      !axy1_2 = wadd_w# (L.mul_c# x1 y2) q0
      !umc1_2 = W.add_w# (L.mul_c# u1 m2) carry1_1
      !(# (# p1, ab1_2 #), c1_2 #) = W.add_c# axy1_2 umc1_2   -- o1, p1, q0, r0
      !carry1_2 = (# ab1_2, c1_2 #)
      -- inner loop, j == 3
      !axy1_3 = wadd_w# (L.mul_c# x1 y3) r0
      !umc1_3 = W.add_w# (L.mul_c# u1 m3) carry1_2
      !(# (# q1, ab1_3 #), c1_3 #) = W.add_c# axy1_3 umc1_3   -- o1, p1, q1, r0
      !carry1_3 = (# ab1_3, c1_3 #)
      -- final stanza
      !(# r1, mc1 #) = wadd_w# carry1_3 mc0                   -- o1, p1, q1, r1
      -- outer loop, i == 2 ---------------------------------------------------
      !axy2 = wadd_w# (L.mul_c# x2 y0) o1
      !u2 = L.mul_w# (lo axy2) n
      !(# (# _, a2 #), c2 #) = W.add_c# (L.mul_c# u2 m0) axy2
      !carry2 = (# a2, c2 #)
      -- inner loop, j == 1
      !axy2_1 = wadd_w# (L.mul_c# x2 y1) p1
      !umc2_1 = W.add_w# (L.mul_c# u2 m1) carry2
      !(# (# o2, ab2_1 #), c2_1 #) = W.add_c# axy2_1 umc2_1   -- o2, p1, q1, r1
      !carry2_1 = (# ab2_1, c2_1 #)
      -- inner loop, j == 2
      !axy2_2 = wadd_w# (L.mul_c# x2 y2) q1
      !umc2_2 = W.add_w# (L.mul_c# u2 m2) carry2_1
      !(# (# p2, ab2_2 #), c2_2 #) = W.add_c# axy2_2 umc2_2   -- o2, p2, q1, r1
      !carry2_2 = (# ab2_2, c2_2 #)
      -- inner loop, j == 3
      !axy2_3 = wadd_w# (L.mul_c# x2 y3) r1
      !umc2_3 = W.add_w# (L.mul_c# u2 m3) carry2_2
      !(# (# q2, ab2_3 #), c2_3 #) = W.add_c# axy2_3 umc2_3   -- o2, p2, q2, r1
      !carry2_3 = (# ab2_3, c2_3 #)
      -- final stanza
      !(# r2, mc2 #) = wadd_w# carry2_3 mc1                   -- o2, p2, q2, r2
      -- outer loop, i == 3 ---------------------------------------------------
      !axy3 = wadd_w# (L.mul_c# x3 y0) o2
      !u3 = L.mul_w# (lo axy3) n
      !(# (# _, a3 #), c3 #) = W.add_c# (L.mul_c# u3 m0) axy3
      !carry3 = (# a3, c3 #)
      -- inner loop, j == 1
      !axy3_1 = wadd_w# (L.mul_c# x3 y1) p2
      !umc3_1 = W.add_w# (L.mul_c# u3 m1) carry3
      !(# (# o3, ab3_1 #), c3_1 #) = W.add_c# axy3_1 umc3_1   -- o3, p2, q2, r2
      !carry3_1 = (# ab3_1, c3_1 #)
      -- inner loop, j == 2
      !axy3_2 = wadd_w# (L.mul_c# x3 y2) q2
      !umc3_2 = W.add_w# (L.mul_c# u3 m2) carry3_1
      !(# (# p3, ab3_2 #), c3_2 #) = W.add_c# axy3_2 umc3_2   -- o3, p3, q1, r2
      !carry3_2 = (# ab3_2, c3_2 #)
      -- inner loop, j == 3
      !axy3_3 = wadd_w# (L.mul_c# x3 y3) r2
      !umc3_3 = W.add_w# (L.mul_c# u3 m3) carry3_2
      !(# (# q3, ab3_3 #), c3_3 #) = W.add_c# axy3_3 umc3_3   -- o3, p3, q3, r2
      !carry3_3 = (# ab3_3, c3_3 #)
      -- final stanza
      !(# r3, mc3 #) = wadd_w# carry3_3 mc2                   -- o3, p3, q3, r3
  in  (# (# o3, p3, q3, r3 #), mc3 #)
{-# INLINE mul_inner# #-}

-- | Montgomery multiplication, with conditional subtract.
mul#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
mul# a b =
  let !m = (# 0xFFFFFFFEFFFFFC2F##, 0xFFFFFFFFFFFFFFFF##    -- field prime
           ,  0xFFFFFFFFFFFFFFFF##, 0xFFFFFFFFFFFFFFFF## #)
      !(# nu, mc #) = mul_inner# a b
  in  WW.sub_mod_c# nu mc m m
{-# NOINLINE mul# #-} -- cannot be inlined without exploding comp time

mul
  :: Montgomery -- ^ lhs in montgomery form
  -> Montgomery -- ^ rhs in montgomery form
  -> Montgomery -- ^ montgomery product
mul (Montgomery a) (Montgomery b) = Montgomery (mul# a b)

to#
  :: (# Word#, Word#, Word#, Word# #) -- ^ integer
  -> (# Word#, Word#, Word#, Word# #)
to# x =
  let !r2 = (# 0x000007A2000E90A1##, 0x1##, 0##, 0## #) -- r^2 mod m
  in mul# x r2
{-# INLINE to# #-}

to :: Wider -> Montgomery
to (Wider x) = Montgomery (to# x)

from :: Montgomery -> Wider
from = retr

-- | Addition in the Montgomery domain.
add#
  :: (# Word#, Word#, Word#, Word# #) -- ^ augend
  -> (# Word#, Word#, Word#, Word# #) -- ^ addend
  -> (# Word#, Word#, Word#, Word# #) -- ^ sum
add# a b =
  let !m = (# 0xFFFFFFFEFFFFFC2F##, 0xFFFFFFFFFFFFFFFF##    -- field prime
           ,  0xFFFFFFFFFFFFFFFF##, 0xFFFFFFFFFFFFFFFF## #)
  in  WW.add_mod# a b m
{-# INLINE add# #-}

add :: Montgomery -> Montgomery -> Montgomery
add (Montgomery a) (Montgomery b) = Montgomery (add# a b)

-- | Subtraction in the Montgomery domain.
sub#
  :: (# Word#, Word#, Word#, Word# #) -- ^ minuend
  -> (# Word#, Word#, Word#, Word# #) -- ^ subtrahend
  -> (# Word#, Word#, Word#, Word# #) -- ^ difference
sub# a b =
  let !m = (# 0xFFFFFFFEFFFFFC2F##, 0xFFFFFFFFFFFFFFFF##    -- field prime
           ,  0xFFFFFFFFFFFFFFFF##, 0xFFFFFFFFFFFFFFFF## #)
  in  WW.sub_mod# a b m
{-# INLINE sub# #-}

sub :: Montgomery -> Montgomery -> Montgomery
sub (Montgomery a) (Montgomery b) = Montgomery (sub# a b)

-- | Modular negation in the Montgomery domain.
neg#
  :: (# Word#, Word#, Word#, Word# #) -- ^ argument
  -> (# Word#, Word#, Word#, Word# #) -- ^ modular negation
neg# a = sub# (# 0##, 0##, 0##, 0## #) a
{-# INLINE neg# #-}

neg :: Montgomery -> Montgomery
neg (Montgomery a) = Montgomery (neg# a)

sqr# :: (# Word#, Word#, Word#, Word# #) -> (# Word#, Word#, Word#, Word# #)
sqr# a =
  let !(# l, h #) = WW.sqr# a
  in  redc# l h
{-# NOINLINE sqr# #-} -- cannot be inlined without exploding comp time

sqr :: Montgomery -> Montgomery
sqr (Montgomery a) = Montgomery (mul# a a)

one :: Montgomery
one = Montgomery (# 0x1000003D1##, 0##, 0##, 0## #)

zero :: Montgomery
zero = Montgomery (# 0##, 0##, 0##, 0## #)

-- generated by etc/generate_inv.sh
inv#
  :: (# Word#, Word#, Word#, Word# #)
  -> (# Word#, Word#, Word#, Word# #)
inv# a =
  let !t0 = (# 0x1000003D1##, 0##, 0##, 0## #) -- montgomery 'one'
      !t1 = sqr# t0
      !t2 = mul# a t1
      !t3 = sqr# t2
      !t4 = mul# a t3
      !t5 = sqr# t4
      !t6 = mul# a t5
      !t7 = sqr# t6
      !t8 = mul# a t7
      !t9 = sqr# t8
      !t10 = mul# a t9
      !t11 = sqr# t10
      !t12 = mul# a t11
      !t13 = sqr# t12
      !t14 = mul# a t13
      !t15 = sqr# t14
      !t16 = mul# a t15
      !t17 = sqr# t16
      !t18 = mul# a t17
      !t19 = sqr# t18
      !t20 = mul# a t19
      !t21 = sqr# t20
      !t22 = mul# a t21
      !t23 = sqr# t22
      !t24 = mul# a t23
      !t25 = sqr# t24
      !t26 = mul# a t25
      !t27 = sqr# t26
      !t28 = mul# a t27
      !t29 = sqr# t28
      !t30 = mul# a t29
      !t31 = sqr# t30
      !t32 = mul# a t31
      !t33 = sqr# t32
      !t34 = mul# a t33
      !t35 = sqr# t34
      !t36 = mul# a t35
      !t37 = sqr# t36
      !t38 = mul# a t37
      !t39 = sqr# t38
      !t40 = mul# a t39
      !t41 = sqr# t40
      !t42 = mul# a t41
      !t43 = sqr# t42
      !t44 = mul# a t43
      !t45 = sqr# t44
      !t46 = mul# a t45
      !t47 = sqr# t46
      !t48 = mul# a t47
      !t49 = sqr# t48
      !t50 = mul# a t49
      !t51 = sqr# t50
      !t52 = mul# a t51
      !t53 = sqr# t52
      !t54 = mul# a t53
      !t55 = sqr# t54
      !t56 = mul# a t55
      !t57 = sqr# t56
      !t58 = mul# a t57
      !t59 = sqr# t58
      !t60 = mul# a t59
      !t61 = sqr# t60
      !t62 = mul# a t61
      !t63 = sqr# t62
      !t64 = mul# a t63
      !t65 = sqr# t64
      !t66 = mul# a t65
      !t67 = sqr# t66
      !t68 = mul# a t67
      !t69 = sqr# t68
      !t70 = mul# a t69
      !t71 = sqr# t70
      !t72 = mul# a t71
      !t73 = sqr# t72
      !t74 = mul# a t73
      !t75 = sqr# t74
      !t76 = mul# a t75
      !t77 = sqr# t76
      !t78 = mul# a t77
      !t79 = sqr# t78
      !t80 = mul# a t79
      !t81 = sqr# t80
      !t82 = mul# a t81
      !t83 = sqr# t82
      !t84 = mul# a t83
      !t85 = sqr# t84
      !t86 = mul# a t85
      !t87 = sqr# t86
      !t88 = mul# a t87
      !t89 = sqr# t88
      !t90 = mul# a t89
      !t91 = sqr# t90
      !t92 = mul# a t91
      !t93 = sqr# t92
      !t94 = mul# a t93
      !t95 = sqr# t94
      !t96 = mul# a t95
      !t97 = sqr# t96
      !t98 = mul# a t97
      !t99 = sqr# t98
      !t100 = mul# a t99
      !t101 = sqr# t100
      !t102 = mul# a t101
      !t103 = sqr# t102
      !t104 = mul# a t103
      !t105 = sqr# t104
      !t106 = mul# a t105
      !t107 = sqr# t106
      !t108 = mul# a t107
      !t109 = sqr# t108
      !t110 = mul# a t109
      !t111 = sqr# t110
      !t112 = mul# a t111
      !t113 = sqr# t112
      !t114 = mul# a t113
      !t115 = sqr# t114
      !t116 = mul# a t115
      !t117 = sqr# t116
      !t118 = mul# a t117
      !t119 = sqr# t118
      !t120 = mul# a t119
      !t121 = sqr# t120
      !t122 = mul# a t121
      !t123 = sqr# t122
      !t124 = mul# a t123
      !t125 = sqr# t124
      !t126 = mul# a t125
      !t127 = sqr# t126
      !t128 = mul# a t127
      !t129 = sqr# t128
      !t130 = mul# a t129
      !t131 = sqr# t130
      !t132 = mul# a t131
      !t133 = sqr# t132
      !t134 = mul# a t133
      !t135 = sqr# t134
      !t136 = mul# a t135
      !t137 = sqr# t136
      !t138 = mul# a t137
      !t139 = sqr# t138
      !t140 = mul# a t139
      !t141 = sqr# t140
      !t142 = mul# a t141
      !t143 = sqr# t142
      !t144 = mul# a t143
      !t145 = sqr# t144
      !t146 = mul# a t145
      !t147 = sqr# t146
      !t148 = mul# a t147
      !t149 = sqr# t148
      !t150 = mul# a t149
      !t151 = sqr# t150
      !t152 = mul# a t151
      !t153 = sqr# t152
      !t154 = mul# a t153
      !t155 = sqr# t154
      !t156 = mul# a t155
      !t157 = sqr# t156
      !t158 = mul# a t157
      !t159 = sqr# t158
      !t160 = mul# a t159
      !t161 = sqr# t160
      !t162 = mul# a t161
      !t163 = sqr# t162
      !t164 = mul# a t163
      !t165 = sqr# t164
      !t166 = mul# a t165
      !t167 = sqr# t166
      !t168 = mul# a t167
      !t169 = sqr# t168
      !t170 = mul# a t169
      !t171 = sqr# t170
      !t172 = mul# a t171
      !t173 = sqr# t172
      !t174 = mul# a t173
      !t175 = sqr# t174
      !t176 = mul# a t175
      !t177 = sqr# t176
      !t178 = mul# a t177
      !t179 = sqr# t178
      !t180 = mul# a t179
      !t181 = sqr# t180
      !t182 = mul# a t181
      !t183 = sqr# t182
      !t184 = mul# a t183
      !t185 = sqr# t184
      !t186 = mul# a t185
      !t187 = sqr# t186
      !t188 = mul# a t187
      !t189 = sqr# t188
      !t190 = mul# a t189
      !t191 = sqr# t190
      !t192 = mul# a t191
      !t193 = sqr# t192
      !t194 = mul# a t193
      !t195 = sqr# t194
      !t196 = mul# a t195
      !t197 = sqr# t196
      !t198 = mul# a t197
      !t199 = sqr# t198
      !t200 = mul# a t199
      !t201 = sqr# t200
      !t202 = mul# a t201
      !t203 = sqr# t202
      !t204 = mul# a t203
      !t205 = sqr# t204
      !t206 = mul# a t205
      !t207 = sqr# t206
      !t208 = mul# a t207
      !t209 = sqr# t208
      !t210 = mul# a t209
      !t211 = sqr# t210
      !t212 = mul# a t211
      !t213 = sqr# t212
      !t214 = mul# a t213
      !t215 = sqr# t214
      !t216 = mul# a t215
      !t217 = sqr# t216
      !t218 = mul# a t217
      !t219 = sqr# t218
      !t220 = mul# a t219
      !t221 = sqr# t220
      !t222 = mul# a t221
      !t223 = sqr# t222
      !t224 = mul# a t223
      !t225 = sqr# t224
      !t226 = mul# a t225
      !t227 = sqr# t226
      !t228 = mul# a t227
      !t229 = sqr# t228
      !t230 = mul# a t229
      !t231 = sqr# t230
      !t232 = mul# a t231
      !t233 = sqr# t232
      !t234 = mul# a t233
      !t235 = sqr# t234
      !t236 = mul# a t235
      !t237 = sqr# t236
      !t238 = mul# a t237
      !t239 = sqr# t238
      !t240 = mul# a t239
      !t241 = sqr# t240
      !t242 = mul# a t241
      !t243 = sqr# t242
      !t244 = mul# a t243
      !t245 = sqr# t244
      !t246 = mul# a t245
      !t247 = sqr# t246
      !t248 = mul# a t247
      !t249 = sqr# t248
      !t250 = mul# a t249
      !t251 = sqr# t250
      !t252 = mul# a t251
      !t253 = sqr# t252
      !t254 = mul# a t253
      !t255 = sqr# t254
      !t256 = mul# a t255
      !t257 = sqr# t256
      !t258 = mul# a t257
      !t259 = sqr# t258
      !t260 = mul# a t259
      !t261 = sqr# t260
      !t262 = mul# a t261
      !t263 = sqr# t262
      !t264 = mul# a t263
      !t265 = sqr# t264
      !t266 = mul# a t265
      !t267 = sqr# t266
      !t268 = mul# a t267
      !t269 = sqr# t268
      !t270 = mul# a t269
      !t271 = sqr# t270
      !t272 = mul# a t271
      !t273 = sqr# t272
      !t274 = mul# a t273
      !t275 = sqr# t274
      !t276 = mul# a t275
      !t277 = sqr# t276
      !t278 = mul# a t277
      !t279 = sqr# t278
      !t280 = mul# a t279
      !t281 = sqr# t280
      !t282 = mul# a t281
      !t283 = sqr# t282
      !t284 = mul# a t283
      !t285 = sqr# t284
      !t286 = mul# a t285
      !t287 = sqr# t286
      !t288 = mul# a t287
      !t289 = sqr# t288
      !t290 = mul# a t289
      !t291 = sqr# t290
      !t292 = mul# a t291
      !t293 = sqr# t292
      !t294 = mul# a t293
      !t295 = sqr# t294
      !t296 = mul# a t295
      !t297 = sqr# t296
      !t298 = mul# a t297
      !t299 = sqr# t298
      !t300 = mul# a t299
      !t301 = sqr# t300
      !t302 = mul# a t301
      !t303 = sqr# t302
      !t304 = mul# a t303
      !t305 = sqr# t304
      !t306 = mul# a t305
      !t307 = sqr# t306
      !t308 = mul# a t307
      !t309 = sqr# t308
      !t310 = mul# a t309
      !t311 = sqr# t310
      !t312 = mul# a t311
      !t313 = sqr# t312
      !t314 = mul# a t313
      !t315 = sqr# t314
      !t316 = mul# a t315
      !t317 = sqr# t316
      !t318 = mul# a t317
      !t319 = sqr# t318
      !t320 = mul# a t319
      !t321 = sqr# t320
      !t322 = mul# a t321
      !t323 = sqr# t322
      !t324 = mul# a t323
      !t325 = sqr# t324
      !t326 = mul# a t325
      !t327 = sqr# t326
      !t328 = mul# a t327
      !t329 = sqr# t328
      !t330 = mul# a t329
      !t331 = sqr# t330
      !t332 = mul# a t331
      !t333 = sqr# t332
      !t334 = mul# a t333
      !t335 = sqr# t334
      !t336 = mul# a t335
      !t337 = sqr# t336
      !t338 = mul# a t337
      !t339 = sqr# t338
      !t340 = mul# a t339
      !t341 = sqr# t340
      !t342 = mul# a t341
      !t343 = sqr# t342
      !t344 = mul# a t343
      !t345 = sqr# t344
      !t346 = mul# a t345
      !t347 = sqr# t346
      !t348 = mul# a t347
      !t349 = sqr# t348
      !t350 = mul# a t349
      !t351 = sqr# t350
      !t352 = mul# a t351
      !t353 = sqr# t352
      !t354 = mul# a t353
      !t355 = sqr# t354
      !t356 = mul# a t355
      !t357 = sqr# t356
      !t358 = mul# a t357
      !t359 = sqr# t358
      !t360 = mul# a t359
      !t361 = sqr# t360
      !t362 = mul# a t361
      !t363 = sqr# t362
      !t364 = mul# a t363
      !t365 = sqr# t364
      !t366 = mul# a t365
      !t367 = sqr# t366
      !t368 = mul# a t367
      !t369 = sqr# t368
      !t370 = mul# a t369
      !t371 = sqr# t370
      !t372 = mul# a t371
      !t373 = sqr# t372
      !t374 = mul# a t373
      !t375 = sqr# t374
      !t376 = mul# a t375
      !t377 = sqr# t376
      !t378 = mul# a t377
      !t379 = sqr# t378
      !t380 = mul# a t379
      !t381 = sqr# t380
      !t382 = mul# a t381
      !t383 = sqr# t382
      !t384 = mul# a t383
      !t385 = sqr# t384
      !t386 = mul# a t385
      !t387 = sqr# t386
      !t388 = mul# a t387
      !t389 = sqr# t388
      !t390 = mul# a t389
      !t391 = sqr# t390
      !t392 = mul# a t391
      !t393 = sqr# t392
      !t394 = mul# a t393
      !t395 = sqr# t394
      !t396 = mul# a t395
      !t397 = sqr# t396
      !t398 = mul# a t397
      !t399 = sqr# t398
      !t400 = mul# a t399
      !t401 = sqr# t400
      !t402 = mul# a t401
      !t403 = sqr# t402
      !t404 = mul# a t403
      !t405 = sqr# t404
      !t406 = mul# a t405
      !t407 = sqr# t406
      !t408 = mul# a t407
      !t409 = sqr# t408
      !t410 = mul# a t409
      !t411 = sqr# t410
      !t412 = mul# a t411
      !t413 = sqr# t412
      !t414 = mul# a t413
      !t415 = sqr# t414
      !t416 = mul# a t415
      !t417 = sqr# t416
      !t418 = mul# a t417
      !t419 = sqr# t418
      !t420 = mul# a t419
      !t421 = sqr# t420
      !t422 = mul# a t421
      !t423 = sqr# t422
      !t424 = mul# a t423
      !t425 = sqr# t424
      !t426 = mul# a t425
      !t427 = sqr# t426
      !t428 = mul# a t427
      !t429 = sqr# t428
      !t430 = mul# a t429
      !t431 = sqr# t430
      !t432 = mul# a t431
      !t433 = sqr# t432
      !t434 = mul# a t433
      !t435 = sqr# t434
      !t436 = mul# a t435
      !t437 = sqr# t436
      !t438 = mul# a t437
      !t439 = sqr# t438
      !t440 = mul# a t439
      !t441 = sqr# t440
      !t442 = mul# a t441
      !t443 = sqr# t442
      !t444 = mul# a t443
      !t445 = sqr# t444
      !t446 = mul# a t445
      !t447 = sqr# t446
      !t448 = sqr# t447
      !t449 = mul# a t448
      !t450 = sqr# t449
      !t451 = mul# a t450
      !t452 = sqr# t451
      !t453 = mul# a t452
      !t454 = sqr# t453
      !t455 = mul# a t454
      !t456 = sqr# t455
      !t457 = mul# a t456
      !t458 = sqr# t457
      !t459 = mul# a t458
      !t460 = sqr# t459
      !t461 = mul# a t460
      !t462 = sqr# t461
      !t463 = mul# a t462
      !t464 = sqr# t463
      !t465 = mul# a t464
      !t466 = sqr# t465
      !t467 = mul# a t466
      !t468 = sqr# t467
      !t469 = mul# a t468
      !t470 = sqr# t469
      !t471 = mul# a t470
      !t472 = sqr# t471
      !t473 = mul# a t472
      !t474 = sqr# t473
      !t475 = mul# a t474
      !t476 = sqr# t475
      !t477 = mul# a t476
      !t478 = sqr# t477
      !t479 = mul# a t478
      !t480 = sqr# t479
      !t481 = mul# a t480
      !t482 = sqr# t481
      !t483 = mul# a t482
      !t484 = sqr# t483
      !t485 = mul# a t484
      !t486 = sqr# t485
      !t487 = mul# a t486
      !t488 = sqr# t487
      !t489 = mul# a t488
      !t490 = sqr# t489
      !t491 = mul# a t490
      !t492 = sqr# t491
      !t493 = sqr# t492
      !t494 = sqr# t493
      !t495 = sqr# t494
      !t496 = sqr# t495
      !t497 = mul# a t496
      !t498 = sqr# t497
      !t499 = sqr# t498
      !t500 = mul# a t499
      !t501 = sqr# t500
      !t502 = mul# a t501
      !t503 = sqr# t502
      !t504 = sqr# t503
      !t505 = mul# a t504
      !r = t505
  in  r
{-# INLINE inv# #-}

inv :: Montgomery -> Montgomery
inv (Montgomery w) = Montgomery (inv# w)
