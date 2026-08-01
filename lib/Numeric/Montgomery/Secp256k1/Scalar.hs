{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- |
-- Module: Numeric.Montgomery.Secp256k1.Scalar
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Montgomery form 'Wider' words, as well as arithmetic operations, with
-- domain derived from the secp256k1 elliptic curve scalar group order.

module Numeric.Montgomery.Secp256k1.Scalar (
  -- * Montgomery form, secp256k1 scalar group order modulus
    Montgomery(..)
  , render
  , to
  , from
  , zero
  , one

  -- * Comparison
  , eq
  , eq_vartime

  -- * Reduction and retrieval
  , redc
  , redc#
  , retr
  , retr#

  -- * Constant-time selection
  , select
  , select#

  -- * Montgomery arithmetic
  , add
  , add#
  , sub
  , sub#
  , mul
  , mul#
  , sqr
  , sqr#
  , neg
  , neg#
  , inv
  , inv#
  , exp
  , exp#
  , odd_vartime
  , odd#
  ) where

import Control.DeepSeq
import qualified Data.Choice as C
import Data.Word.Limb (Limb(..))
import qualified Data.Word.Limb as L
import qualified Data.Word.Wide as W
import Data.Word.Wider (Wider(..))
import qualified Data.Word.Wider as WW
import GHC.Exts (Word(..), Word#)
import Prelude hiding (or, and, not, exp)

-- montgomery arithmetic, specialized to the secp256k1 scalar group order
-- 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

-- | Montgomery-form 'Wider' words, on the Montgomery domain defined by
--   the secp256k1 scalar group order.
--
--   >>> let one = 1 :: Montgomery
--   >>> one
--   1
--   >>> putStrLn (render one)
--   (4624529908474429119, 4994812053365940164, 1, 0)
data Montgomery = Montgomery !Limb4

instance Show Montgomery where
  show = show . from

-- | Render a 'Montgomery' value as a 'String', showing its individual
--   'Limb's.
--
--   >>> putStrLn (render 1)
--   (4624529908474429119, 4994812053365940164, 1, 0)
render :: Montgomery -> String
render (Montgomery (L4 a b c d)) =
     "(" <> show (W# a) <> ", " <> show (W# b) <> ", "
  <> show (W# c) <> ", " <> show (W# d) <> ")"

-- | Note that 'fromInteger' necessarily runs in variable time due
--   to conversion from the variable-size, potentially heap-allocated
--   'Integer' type.
instance Num Montgomery where
  a + b = add a b
  a - b = sub a b
  a * b = mul a b
  negate a = neg a
  abs = id
  fromInteger = to . WW.to_vartime
  signum (Montgomery (# l0, l1, l2, l3 #)) =
    let !(Limb l) = l0 `L.or#` l1 `L.or#` l2 `L.or#` l3
        !n = C.from_word_nonzero# l
        !b = C.to_word# n
    in  Montgomery (L4 b 0## 0## 0##)

instance NFData Montgomery where
  rnf (Montgomery a) = case a of (# _, _, _, _ #) -> ()

-- utilities ------------------------------------------------------------------

type Limb2 = (# Limb, Limb #)

type Limb4 = (# Limb, Limb, Limb, Limb #)

pattern L4 :: Word# -> Word# -> Word# -> Word# -> Limb4
pattern L4 w0 w1 w2 w3 = (# Limb w0, Limb w1, Limb w2, Limb w3 #)
{-# COMPLETE L4 #-}

-- Wide wrapping addition, when addend is only a limb.
wadd_w# :: Limb2 -> Limb -> Limb2
wadd_w# (# x_lo, x_hi #) y_lo =
  let !(# s0, c0 #) = L.add_o# x_lo y_lo
      !(# s1, _ #) = L.add_o# x_hi c0
  in  (# s0, s1 #)
{-# INLINE wadd_w# #-}

-- Truncate a wide word to a 'Limb'.
lo :: Limb2 -> Limb
lo (# l, _ #) = l
{-# INLINE lo #-}

-- comparison -----------------------------------------------------------------

-- | Constant-time equality comparison.
eq :: Montgomery -> Montgomery -> C.Choice
eq (Montgomery (L4 a0 a1 a2 a3)) (Montgomery (L4 b0 b1 b2 b3)) =
  C.eq_wider# (# a0, a1, a2, a3 #) (# b0, b1, b2, b3 #)
{-# INLINE eq #-}

-- | Variable-time equality comparison.
eq_vartime :: Montgomery -> Montgomery -> Bool
eq_vartime (Montgomery (Wider -> a)) (Montgomery (Wider -> b)) =
  WW.eq_vartime a b

-- innards --------------------------------------------------------------------

redc_inner#
  :: Limb4             -- ^ upper limbs
  -> Limb4             -- ^ lower limbs
  -> (# Limb4, Limb #) -- ^ upper limbs, meta-carry
redc_inner# (# u0, u1, u2, u3 #) (# l0, l1, l2, l3 #) =
  let !(# m0, m1, m2, m3 #) =
        L4 0xBFD25E8CD0364141## 0xBAAEDCE6AF48A03B##
           0xFFFFFFFFFFFFFFFE## 0xFFFFFFFFFFFFFFFF##
      !n                = Limb 0x4B0DFF665588B13F##
      !w_0              = L.mul_w# l0 n
      !(# _, c_00 #)    = L.mac# w_0 m0 l0 (Limb 0##)
      !(# l0_1, c_01 #) = L.mac# w_0 m1 l1 c_00
      !(# l0_2, c_02 #) = L.mac# w_0 m2 l2 c_01
      !(# l0_3, c_03 #) = L.mac# w_0 m3 l3 c_02
      !(# u_0, mc_0 #)  = L.add_c# u0 c_03 (Limb 0##)
      !w_1              = L.mul_w# l0_1 n
      !(# _, c_10 #)    = L.mac# w_1 m0 l0_1 (Limb 0##)
      !(# l1_1, c_11 #) = L.mac# w_1 m1 l0_2 c_10
      !(# l1_2, c_12 #) = L.mac# w_1 m2 l0_3 c_11
      !(# u1_3, c_13 #) = L.mac# w_1 m3 u_0 c_12
      !(# u_1, mc_1 #)  = L.add_c# u1 c_13 mc_0
      !w_2              = L.mul_w# l1_1 n
      !(# _, c_20 #)    = L.mac# w_2 m0 l1_1 (Limb 0##)
      !(# l2_1, c_21 #) = L.mac# w_2 m1 l1_2 c_20
      !(# u2_2, c_22 #) = L.mac# w_2 m2 u1_3 c_21
      !(# u2_3, c_23 #) = L.mac# w_2 m3 u_1 c_22
      !(# u_2, mc_2 #)  = L.add_c# u2 c_23 mc_1
      !w_3              = L.mul_w# l2_1 n
      !(# _, c_30 #)    = L.mac# w_3 m0 l2_1 (Limb 0##)
      !(# u3_1, c_31 #) = L.mac# w_3 m1 u2_2 c_30
      !(# u3_2, c_32 #) = L.mac# w_3 m2 u2_3 c_31
      !(# u3_3, c_33 #) = L.mac# w_3 m3 u_2 c_32
      !(# u_3, mc_3 #)  = L.add_c# u3 c_33 mc_2
  in  (# (# u3_1, u3_2, u3_3, u_3 #), mc_3 #)
{-# INLINE redc_inner# #-}

redc#
  :: Limb4 -- ^ lower limbs
  -> Limb4 -- ^ upper limbs
  -> Limb4 -- ^ result
redc# l u =
  let -- group order
      !m = L4 0xBFD25E8CD0364141## 0xBAAEDCE6AF48A03B##
              0xFFFFFFFFFFFFFFFE## 0xFFFFFFFFFFFFFFFF##
      !(# nu, mc #) = redc_inner# u l
  in  WW.sub_mod_c# nu mc m m
{-# INLINE redc# #-}

-- | Montgomery reduction.
--
--   The first argument represents the low words, and the second the
--   high words, of an extra-large eight-limb word in Montgomery form.
redc
  :: Montgomery -- ^ low wider-word, Montgomery form
  -> Montgomery -- ^ high wider-word, Montgomery form
  -> Montgomery -- ^ reduced value
redc (Montgomery l) (Montgomery u) =
  let !res = redc# l u
  in  (Montgomery res)

retr_inner#
  :: Limb4 -- ^ value in montgomery form
  -> Limb4 -- ^ retrieved value
retr_inner# (# x0, x1, x2, x3 #) =
  let !(# m0, m1, m2, m3 #) =
        L4 0xBFD25E8CD0364141## 0xBAAEDCE6AF48A03B##
           0xFFFFFFFFFFFFFFFE## 0xFFFFFFFFFFFFFFFF##
      !n                = Limb 0x4B0DFF665588B13F##
      !u_0              = L.mul_w# x0 n
      !(# _, o0 #)      = L.mac# u_0 m0 x0 (Limb 0##)
      !(# o0_1, p0_1 #) = L.mac# u_0 m1 (Limb 0##) o0
      !(# p0_2, q0_2 #) = L.mac# u_0 m2 (Limb 0##) p0_1
      !(# q0_3, r0_3 #) = L.mac# u_0 m3 (Limb 0##) q0_2
      !u_1              = L.mul_w# (L.add_w# o0_1 x1) n
      !(# _, o1 #)      = L.mac# u_1 m0 x1 o0_1
      !(# o1_1, p1_1 #) = L.mac# u_1 m1 p0_2 o1
      !(# p1_2, q1_2 #) = L.mac# u_1 m2 q0_3 p1_1
      !(# q1_3, r1_3 #) = L.mac# u_1 m3 r0_3 q1_2
      !u_2              = L.mul_w# (L.add_w# o1_1 x2) n
      !(# _, o2 #)      = L.mac# u_2 m0 x2 o1_1
      !(# o2_1, p2_1 #) = L.mac# u_2 m1 p1_2 o2
      !(# p2_2, q2_2 #) = L.mac# u_2 m2 q1_3 p2_1
      !(# q2_3, r2_3 #) = L.mac# u_2 m3 r1_3 q2_2
      !u_3              = L.mul_w# (L.add_w# o2_1 x3) n
      !(# _, o3 #)      = L.mac# u_3 m0 x3 o2_1
      !(# o3_1, p3_1 #) = L.mac# u_3 m1 p2_2 o3
      !(# p3_2, q3_2 #) = L.mac# u_3 m2 q2_3 p3_1
      !(# q3_3, r3_3 #) = L.mac# u_3 m3 r2_3 q3_2
  in  (# o3_1, p3_2, q3_3, r3_3 #)
{-# INLINE retr_inner# #-}

retr#
  :: Limb4
  -> Limb4
retr# f = retr_inner# f
{-# INLINE retr# #-}

-- | Retrieve a 'Montgomery' value from the Montgomery domain, producing
--   a 'Wider' word.
retr
  :: Montgomery -- ^ value in Montgomery form
  -> Wider      -- ^ retrieved value
retr (Montgomery f) =
  let !res = retr# f
  in  (Wider res)

-- | Montgomery multiplication (FIOS), without conditional subtract.
mul_inner#
  :: Limb4              -- ^ x
  -> Limb4              -- ^ y
  -> (# Limb4, Limb #)  -- ^ product, meta-carry
mul_inner# (# x0, x1, x2, x3 #) (# y0, y1, y2, y3 #) =
  let !(# m0, m1, m2, m3 #) =
        L4 0xBFD25E8CD0364141## 0xBAAEDCE6AF48A03B##
           0xFFFFFFFFFFFFFFFE## 0xFFFFFFFFFFFFFFFF##
      !n                           = Limb 0x4B0DFF665588B13F##
      !axy0                        = L.mul_c# x0 y0
      !u0                          = L.mul_w# (lo axy0) n
      !(# (# _, a0 #), c0 #)       = W.add_o# (L.mul_c# u0 m0) axy0
      !carry0                      = (# a0, c0 #)
      !axy0_1                      = L.mul_c# x0 y1
      !umc0_1                      = W.add_w# (L.mul_c# u0 m1) carry0
      !(# (# o0, ab0_1 #), c0_1 #) = W.add_o# axy0_1 umc0_1
      !carry0_1                    = (# ab0_1, c0_1 #)
      !axy0_2                      = L.mul_c# x0 y2
      !umc0_2                      = W.add_w# (L.mul_c# u0 m2) carry0_1
      !(# (# p0, ab0_2 #), c0_2 #) = W.add_o# axy0_2 umc0_2
      !carry0_2                    = (# ab0_2, c0_2 #)
      !axy0_3                      = L.mul_c# x0 y3
      !umc0_3                      = W.add_w# (L.mul_c# u0 m3) carry0_2
      !(# (# q0, ab0_3 #), c0_3 #) = W.add_o# axy0_3 umc0_3
      !carry0_3                    = (# ab0_3, c0_3 #)
      !(# r0, mc0 #)               = carry0_3
      !axy1                        = wadd_w# (L.mul_c# x1 y0) o0
      !u1                          = L.mul_w# (lo axy1) n
      !(# (# _, a1 #), c1 #)       = W.add_o# (L.mul_c# u1 m0) axy1
      !carry1                      = (# a1, c1 #)
      !axy1_1                      = wadd_w# (L.mul_c# x1 y1) p0
      !umc1_1                      = W.add_w# (L.mul_c# u1 m1) carry1
      !(# (# o1, ab1_1 #), c1_1 #) = W.add_o# axy1_1 umc1_1
      !carry1_1                    = (# ab1_1, c1_1 #)
      !axy1_2                      = wadd_w# (L.mul_c# x1 y2) q0
      !umc1_2                      = W.add_w# (L.mul_c# u1 m2) carry1_1
      !(# (# p1, ab1_2 #), c1_2 #) = W.add_o# axy1_2 umc1_2
      !carry1_2                    = (# ab1_2, c1_2 #)
      !axy1_3                      = wadd_w# (L.mul_c# x1 y3) r0
      !umc1_3                      = W.add_w# (L.mul_c# u1 m3) carry1_2
      !(# (# q1, ab1_3 #), c1_3 #) = W.add_o# axy1_3 umc1_3
      !carry1_3                    = (# ab1_3, c1_3 #)
      !(# r1, mc1 #)               = wadd_w# carry1_3 mc0
      !axy2                        = wadd_w# (L.mul_c# x2 y0) o1
      !u2                          = L.mul_w# (lo axy2) n
      !(# (# _, a2 #), c2 #)       = W.add_o# (L.mul_c# u2 m0) axy2
      !carry2                      = (# a2, c2 #)
      !axy2_1                      = wadd_w# (L.mul_c# x2 y1) p1
      !umc2_1                      = W.add_w# (L.mul_c# u2 m1) carry2
      !(# (# o2, ab2_1 #), c2_1 #) = W.add_o# axy2_1 umc2_1
      !carry2_1                    = (# ab2_1, c2_1 #)
      !axy2_2                      = wadd_w# (L.mul_c# x2 y2) q1
      !umc2_2                      = W.add_w# (L.mul_c# u2 m2) carry2_1
      !(# (# p2, ab2_2 #), c2_2 #) = W.add_o# axy2_2 umc2_2
      !carry2_2                    = (# ab2_2, c2_2 #)
      !axy2_3                      = wadd_w# (L.mul_c# x2 y3) r1
      !umc2_3                      = W.add_w# (L.mul_c# u2 m3) carry2_2
      !(# (# q2, ab2_3 #), c2_3 #) = W.add_o# axy2_3 umc2_3
      !carry2_3                    = (# ab2_3, c2_3 #)
      !(# r2, mc2 #)               = wadd_w# carry2_3 mc1
      !axy3                        = wadd_w# (L.mul_c# x3 y0) o2
      !u3                          = L.mul_w# (lo axy3) n
      !(# (# _, a3 #), c3 #)       = W.add_o# (L.mul_c# u3 m0) axy3
      !carry3                      = (# a3, c3 #)
      !axy3_1                      = wadd_w# (L.mul_c# x3 y1) p2
      !umc3_1                      = W.add_w# (L.mul_c# u3 m1) carry3
      !(# (# o3, ab3_1 #), c3_1 #) = W.add_o# axy3_1 umc3_1
      !carry3_1                    = (# ab3_1, c3_1 #)
      !axy3_2                      = wadd_w# (L.mul_c# x3 y2) q2
      !umc3_2                      = W.add_w# (L.mul_c# u3 m2) carry3_1
      !(# (# p3, ab3_2 #), c3_2 #) = W.add_o# axy3_2 umc3_2
      !carry3_2                    = (# ab3_2, c3_2 #)
      !axy3_3                      = wadd_w# (L.mul_c# x3 y3) r2
      !umc3_3                      = W.add_w# (L.mul_c# u3 m3) carry3_2
      !(# (# q3, ab3_3 #), c3_3 #) = W.add_o# axy3_3 umc3_3
      !carry3_3                    = (# ab3_3, c3_3 #)
      !(# r3, mc3 #)               = wadd_w# carry3_3 mc2
  in  (# (# o3, p3, q3, r3 #), mc3 #)
{-# INLINE mul_inner# #-}

mul#
  :: Limb4
  -> Limb4
  -> Limb4
mul# a b =
  let -- group order
      !m = L4 0xBFD25E8CD0364141## 0xBAAEDCE6AF48A03B##
              0xFFFFFFFFFFFFFFFE## 0xFFFFFFFFFFFFFFFF##
      !(# nu, mc #) = mul_inner# a b
  in  WW.sub_mod_c# nu mc m m
{-# NOINLINE mul# #-} -- cannot be inlined without exploding comp time

-- | Multiplication in the Montgomery domain.
--
--   Note that 'Montgomery' is an instance of 'Num', so you can use '*'
--   to apply this function.
--
--   >>> 1 * 1 :: Montgomery
--   1
mul
  :: Montgomery -- ^ multiplicand in montgomery form
  -> Montgomery -- ^ multiplier in montgomery form
  -> Montgomery -- ^ montgomery product
mul (Montgomery a) (Montgomery b) = Montgomery (mul# a b)

to#
  :: Limb4 -- ^ integer
  -> Limb4
to# x =
  let !r2 = L4 0x896CF21467D7D140## 0x741496C20E7CF878## -- r^2 mod m
               0xE697F5E45BCD07C6## 0x9D671CD581C69BC5##
  in  mul# x r2
{-# INLINE to# #-}

-- | Convert a 'Wider' word to the Montgomery domain.
to :: Wider -> Montgomery
to (Wider x) = Montgomery (to# x)

-- | Retrieve a 'Montgomery' word from the Montgomery domain.
--
--   This function is a synonym for 'retr'.
from :: Montgomery -> Wider
from = retr

add#
  :: Limb4 -- ^ augend
  -> Limb4 -- ^ addend
  -> Limb4 -- ^ sum
add# a b =
  let -- group order
      !m = L4 0xBFD25E8CD0364141## 0xBAAEDCE6AF48A03B##
              0xFFFFFFFFFFFFFFFE## 0xFFFFFFFFFFFFFFFF##
  in  WW.add_mod# a b m
{-# INLINE add# #-}

-- | Addition in the Montgomery domain.
--
--   Note that 'Montgomery' is an instance of 'Num', so you can use '+'
--   to apply this function.
--
--   >>> 1 + 1 :: Montgomery
--   2
add
  :: Montgomery -- ^ augend
  -> Montgomery -- ^ addend
  -> Montgomery -- ^ sum
add (Montgomery a) (Montgomery b) = Montgomery (add# a b)

sub#
  :: Limb4 -- ^ minuend
  -> Limb4 -- ^ subtrahend
  -> Limb4 -- ^ difference
sub# a b =
  let !m = L4 0xBFD25E8CD0364141## 0xBAAEDCE6AF48A03B##
              0xFFFFFFFFFFFFFFFE## 0xFFFFFFFFFFFFFFFF##
  in  WW.sub_mod# a b m
{-# INLINE sub# #-}

-- | Subtraction in the Montgomery domain.
--
--   Note that 'Montgomery' is an instance of 'Num', so you can use '-'
--   to apply this function.
--
--   >>> 1 - 1 :: Montgomery
--   0
sub
  :: Montgomery -- ^ minuend
  -> Montgomery -- ^ subtrahend
  -> Montgomery -- ^ difference
sub (Montgomery a) (Montgomery b) = Montgomery (sub# a b)

neg#
  :: Limb4 -- ^ argument
  -> Limb4 -- ^ modular negation
neg# a = sub# (L4 0## 0## 0## 0##) a
{-# INLINE neg# #-}

-- | Additive inverse in the Montgomery domain.
--
--   Note that 'Montgomery' is an instance of 'Num', so you can use 'negate'
--   to apply this function.
--
--   >>> negate 1 :: Montgomery
--   115792089237316195423570985008687907852837564279074904382605163141518161494336
--   >>> (negate 1 :: Montgomery) + 1
--   0
neg :: Montgomery -> Montgomery
neg (Montgomery a) = Montgomery (neg# a)

sqr# :: Limb4 -> Limb4
sqr# a =
  let !(# l, h #) = WW.sqr# a
  in  redc# l h
{-# NOINLINE sqr# #-} -- cannot be inlined without exploding comp time

-- | Squaring in the Montgomery domain.
--
--   >>> sqr 1
--   1
--   >>> sqr 2
--   4
--   >>> sqr (negate 2)
--   4
sqr
  :: Montgomery -- ^ argument
  -> Montgomery -- ^ square
sqr (Montgomery a) = Montgomery (mul# a a)

-- | Zero (the additive unit) in the Montgomery domain.
zero :: Montgomery
zero = Montgomery (L4 0## 0## 0## 0##)

-- | One (the multiplicative unit) in the Montgomery domain.
one :: Montgomery
one = Montgomery (L4 0x402DA1732FC9BEBF## 0x4551231950B75FC4##
                     0x0000000000000001## 0x0000000000000000##)

-- generated by etc/generate_inv.sh
inv#
  :: Limb4
  -> Limb4
inv# a =
  let
      !t1 = sqr# a
      !t2 = mul# t1 a
      !t3 = sqr# t2
      !t4 = sqr# t3
      !t5 = mul# t4 t2
      !t6 = sqr# t5
      !t7 = sqr# t6
      !t8 = sqr# t7
      !t9 = sqr# t8
      !t10 = mul# t9 t5
      !t11 = sqr# t10
      !t12 = sqr# t11
      !t13 = sqr# t12
      !t14 = sqr# t13
      !t15 = sqr# t14
      !t16 = sqr# t15
      !t17 = sqr# t16
      !t18 = sqr# t17
      !t19 = mul# t18 t10
      !t20 = sqr# t19
      !t21 = sqr# t20
      !t22 = sqr# t21
      !t23 = sqr# t22
      !t24 = sqr# t23
      !t25 = sqr# t24
      !t26 = sqr# t25
      !t27 = sqr# t26
      !t28 = sqr# t27
      !t29 = sqr# t28
      !t30 = sqr# t29
      !t31 = sqr# t30
      !t32 = sqr# t31
      !t33 = sqr# t32
      !t34 = sqr# t33
      !t35 = sqr# t34
      !t36 = mul# t35 t19
      !t37 = sqr# t36
      !t38 = sqr# t37
      !t39 = sqr# t38
      !t40 = sqr# t39
      !t41 = sqr# t40
      !t42 = sqr# t41
      !t43 = sqr# t42
      !t44 = sqr# t43
      !t45 = sqr# t44
      !t46 = sqr# t45
      !t47 = sqr# t46
      !t48 = sqr# t47
      !t49 = sqr# t48
      !t50 = sqr# t49
      !t51 = sqr# t50
      !t52 = sqr# t51
      !t53 = sqr# t52
      !t54 = sqr# t53
      !t55 = sqr# t54
      !t56 = sqr# t55
      !t57 = sqr# t56
      !t58 = sqr# t57
      !t59 = sqr# t58
      !t60 = sqr# t59
      !t61 = sqr# t60
      !t62 = sqr# t61
      !t63 = sqr# t62
      !t64 = sqr# t63
      !t65 = sqr# t64
      !t66 = sqr# t65
      !t67 = sqr# t66
      !t68 = sqr# t67
      !t69 = mul# t68 t36
      !t70 = sqr# t69
      !t71 = sqr# t70
      !t72 = sqr# t71
      !t73 = sqr# t72
      !t74 = sqr# t73
      !t75 = sqr# t74
      !t76 = sqr# t75
      !t77 = sqr# t76
      !t78 = sqr# t77
      !t79 = sqr# t78
      !t80 = sqr# t79
      !t81 = sqr# t80
      !t82 = sqr# t81
      !t83 = sqr# t82
      !t84 = sqr# t83
      !t85 = sqr# t84
      !t86 = sqr# t85
      !t87 = sqr# t86
      !t88 = sqr# t87
      !t89 = sqr# t88
      !t90 = sqr# t89
      !t91 = sqr# t90
      !t92 = sqr# t91
      !t93 = sqr# t92
      !t94 = sqr# t93
      !t95 = sqr# t94
      !t96 = sqr# t95
      !t97 = sqr# t96
      !t98 = sqr# t97
      !t99 = sqr# t98
      !t100 = sqr# t99
      !t101 = sqr# t100
      !t102 = mul# t101 t36
      !t103 = sqr# t102
      !t104 = sqr# t103
      !t105 = sqr# t104
      !t106 = sqr# t105
      !t107 = sqr# t106
      !t108 = sqr# t107
      !t109 = sqr# t108
      !t110 = sqr# t109
      !t111 = sqr# t110
      !t112 = sqr# t111
      !t113 = sqr# t112
      !t114 = sqr# t113
      !t115 = sqr# t114
      !t116 = sqr# t115
      !t117 = sqr# t116
      !t118 = sqr# t117
      !t119 = mul# t118 t19
      !t120 = sqr# t119
      !t121 = sqr# t120
      !t122 = sqr# t121
      !t123 = sqr# t122
      !t124 = sqr# t123
      !t125 = sqr# t124
      !t126 = sqr# t125
      !t127 = sqr# t126
      !t128 = mul# t127 t10
      !t129 = sqr# t128
      !t130 = sqr# t129
      !t131 = sqr# t130
      !t132 = sqr# t131
      !t133 = mul# t132 t5
      !t134 = sqr# t133
      !t135 = sqr# t134
      !t136 = mul# t135 t2
      !t137 = sqr# t136
      !t138 = mul# t137 a
      !t139 = sqr# t2
      !t140 = mul# t139 a
      !t141 = sqr# t5
      !t142 = sqr# t141
      !t143 = mul# t142 t2
      !t144 = sqr# t138
      !t145 = sqr# t144
      !t146 = mul# t145 a
      !t147 = sqr# t146
      !t148 = sqr# t147
      !t149 = sqr# t148
      !t150 = sqr# t149
      !t151 = mul# t150 t140
      !t152 = sqr# t151
      !t153 = sqr# t152
      !t154 = mul# t153 a
      !t155 = sqr# t154
      !t156 = sqr# t155
      !t157 = mul# t156 a
      !t158 = sqr# t157
      !t159 = sqr# t158
      !t160 = mul# t159 a
      !t161 = sqr# t160
      !t162 = sqr# t161
      !t163 = sqr# t162
      !t164 = sqr# t163
      !t165 = mul# t164 t140
      !t166 = sqr# t165
      !t167 = sqr# t166
      !t168 = sqr# t167
      !t169 = mul# t168 t2
      !t170 = sqr# t169
      !t171 = sqr# t170
      !t172 = sqr# t171
      !t173 = sqr# t172
      !t174 = mul# t173 t140
      !t175 = sqr# t174
      !t176 = sqr# t175
      !t177 = sqr# t176
      !t178 = sqr# t177
      !t179 = sqr# t178
      !t180 = mul# t179 t140
      !t181 = sqr# t180
      !t182 = sqr# t181
      !t183 = sqr# t182
      !t184 = sqr# t183
      !t185 = mul# t184 t2
      !t186 = sqr# t185
      !t187 = sqr# t186
      !t188 = mul# t187 a
      !t189 = sqr# t188
      !t190 = sqr# t189
      !t191 = mul# t190 a
      !t192 = sqr# t191
      !t193 = sqr# t192
      !t194 = sqr# t193
      !t195 = sqr# t194
      !t196 = sqr# t195
      !t197 = mul# t196 t5
      !t198 = sqr# t197
      !t199 = sqr# t198
      !t200 = mul# t199 a
      !t201 = sqr# t200
      !t202 = sqr# t201
      !t203 = sqr# t202
      !t204 = mul# t203 a
      !t205 = sqr# t204
      !t206 = sqr# t205
      !t207 = sqr# t206
      !t208 = sqr# t207
      !t209 = mul# t208 a
      !t210 = sqr# t209
      !t211 = sqr# t210
      !t212 = mul# t211 a
      !t213 = sqr# t212
      !t214 = sqr# t213
      !t215 = sqr# t214
      !t216 = sqr# t215
      !t217 = sqr# t216
      !t218 = sqr# t217
      !t219 = sqr# t218
      !t220 = sqr# t219
      !t221 = sqr# t220
      !t222 = sqr# t221
      !t223 = mul# t222 t140
      !t224 = sqr# t223
      !t225 = sqr# t224
      !t226 = sqr# t225
      !t227 = sqr# t226
      !t228 = mul# t227 t140
      !t229 = sqr# t228
      !t230 = sqr# t229
      !t231 = sqr# t230
      !t232 = sqr# t231
      !t233 = sqr# t232
      !t234 = sqr# t233
      !t235 = sqr# t234
      !t236 = sqr# t235
      !t237 = sqr# t236
      !t238 = mul# t237 t10
      !t239 = sqr# t238
      !t240 = sqr# t239
      !t241 = mul# t240 a
      !t242 = sqr# t241
      !t243 = sqr# t242
      !t244 = sqr# t243
      !t245 = mul# t244 a
      !t246 = sqr# t245
      !t247 = sqr# t246
      !t248 = sqr# t247
      !t249 = mul# t248 a
      !t250 = sqr# t249
      !t251 = sqr# t250
      !t252 = sqr# t251
      !t253 = sqr# t252
      !t254 = sqr# t253
      !t255 = mul# t254 t5
      !t256 = sqr# t255
      !t257 = sqr# t256
      !t258 = mul# t257 a
      !t259 = sqr# t258
      !t260 = sqr# t259
      !t261 = sqr# t260
      !t262 = sqr# t261
      !t263 = sqr# t262
      !t264 = mul# t263 t2
      !t265 = sqr# t264
      !t266 = sqr# t265
      !t267 = sqr# t266
      !t268 = sqr# t267
      !t269 = mul# t268 t2
      !t270 = sqr# t269
      !t271 = sqr# t270
      !t272 = mul# t271 a
      !t273 = sqr# t272
      !t274 = sqr# t273
      !t275 = sqr# t274
      !t276 = sqr# t275
      !t277 = sqr# t276
      !t278 = sqr# t277
      !t279 = sqr# t278
      !t280 = sqr# t279
      !t281 = mul# t280 t2
      !t282 = sqr# t281
      !t283 = sqr# t282
      !t284 = sqr# t283
      !t285 = mul# t284 t2
      !t286 = sqr# t285
      !t287 = sqr# t286
      !t288 = sqr# t287
      !t289 = mul# t288 a
      !t290 = sqr# t289
      !t291 = sqr# t290
      !t292 = sqr# t291
      !t293 = sqr# t292
      !t294 = sqr# t293
      !t295 = sqr# t294
      !t296 = mul# t295 a
      !t297 = sqr# t296
      !t298 = sqr# t297
      !t299 = sqr# t298
      !t300 = sqr# t299
      !t301 = sqr# t300
      !t302 = sqr# t301
      !t303 = sqr# t302
      !t304 = sqr# t303
      !t305 = mul# t304 t143
      !r = t305
  in  r
{-# INLINE inv# #-}

-- | Multiplicative inverse in the Montgomery domain.
--
--   >> inv 2
--   57896044618658097711785492504343953926418782139537452191302581570759080747169
--   >> inv 2 * 2
--   1
inv
  :: Montgomery -- ^ argument
  -> Montgomery -- ^ inverse
inv (Montgomery w) = Montgomery (inv# w)

-- | Exponentiation in the Montgomery domain.
--
--   >>> exp 2 3
--   8
--   >>> exp 2 10
--   1024
exp :: Montgomery -> Wider -> Montgomery
exp (Montgomery b) (Wider e) = Montgomery (exp# b e)

exp#
  :: Limb4
  -> Limb4
  -> Limb4
exp# b e =
  let !o = L4 0x402DA1732FC9BEBF## 0x4551231950B75FC4##
              0x0000000000000001## 0x0000000000000000##
      loop !r !m !ex n = case n of
        0 -> r
        _ ->
          let !(# ne, bit #) = WW.shr1_c# ex
              !candidate = mul# r m
              !nr = select# r candidate bit
              !nm = sqr# m
          in  loop nr nm ne (n - 1)
  in  loop o b e (256 :: Word)
{-# INLINE exp# #-}

odd# :: Limb4 -> C.Choice
odd# = WW.odd#
{-# INLINE odd# #-}

-- | Check if a 'Montgomery' value is odd.
--
--   Note that the comparison is performed in constant time, but we
--   branch when converting to 'Bool'.
--
--   >>> odd 1
--   True
--   >>> odd 2
--   False
--   >>> Data.Word.Wider.odd (retr 3) -- parity is preserved
--   True
odd_vartime :: Montgomery -> Bool
odd_vartime (Montgomery m) = C.decide (odd# m)

-- constant-time selection ----------------------------------------------------

select#
  :: Limb4    -- ^ a
  -> Limb4    -- ^ b
  -> C.Choice -- ^ c
  -> Limb4    -- ^ result
select# = WW.select#
{-# INLINE select# #-}

-- | Return b if c is truthy, otherwise return a.
--
--   >>> import qualified Data.Choice as C
--   >>> select 0 1 (C.true# ())
--   1
select
  :: Montgomery    -- ^ a
  -> Montgomery    -- ^ b
  -> C.Choice      -- ^ c
  -> Montgomery    -- ^ result
select (Montgomery a) (Montgomery b) c = Montgomery (select# a b c)

