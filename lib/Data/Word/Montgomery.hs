{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Word.Montgomery where

import qualified Data.Word.Limb as L
import qualified Data.Word.Wide as W
import Data.Word.Wider (Wider(..))
import qualified Data.Word.Wider as WW
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- utilities ------------------------------------------------------------------

-- Convert an unboxed word to an unboxed wide word.
w :: Word# -> (# Word#, Word# #)
w m = (# m, 0## #)
{-# INLINE w #-}

-- Truncate an unboxed wide word to an unboxed word.
lo :: (# Word#, Word# #) -> Word#
lo (# l, _ #) = l
{-# INLINE lo #-}

-- Capture the high bits of an unboxed wide word.
hi :: (# Word#, Word# #) -> Word#
hi (# _, h #) = h
{-# INLINE hi #-}

-- innards --------------------------------------------------------------------

redc_inner#
  :: (# Word#, Word#, Word#, Word# #)              -- ^ upper words
  -> (# Word#, Word#, Word#, Word# #)              -- ^ lower words
  -> (# Word#, Word#, Word#, Word# #)              -- ^ modulus
  -> Word#                                         -- ^ mod neg inv
  -> (# (# Word#, Word#, Word#, Word# #), Word# #) -- ^ upper words, meta-carry
redc_inner# (# u0, u1, u2, u3 #) (# l0, l1, l2, l3 #) (# m0, m1, m2, m3 #) n =
  let -- outer loop, i == 0 ---------------------------------------------------
      !w_0 = L.mul_w# l0 n
      !(# _, c_00 #) = L.mul_add_c# w_0 m0 l0 0##     -- m0, l0
      -- first inner loop (j < 4)
      !(# l0_1, c_01 #) = L.mul_add_c# w_0 m1 l1 c_00 -- l<i idx>_<j idx>
      !(# l0_2, c_02 #) = L.mul_add_c# w_0 m2 l2 c_01
      !(# l0_3, c_03 #) = L.mul_add_c# w_0 m3 l3 c_02
      -- final stanza
      !(# u_0, mc_0 #) = L.add_c# u0 c_03 0##
      -- end states
      -- (# l0, l0_1, l0_2, l0_3 #)
      -- (# u_0, u1, u2, u3 #)
      -- outer loop, i == 1 ---------------------------------------------------
      !w_1 = L.mul_w# l0_1 n
      !(# _, c_10 #) = L.mul_add_c# w_1 m0 l0_1 0##
      -- first inner loop (j < 3)
      !(# l1_1, c_11 #) = L.mul_add_c# w_1 m1 l0_2 c_10 -- j == 1
      !(# l1_2, c_12 #) = L.mul_add_c# w_1 m2 l0_3 c_11 -- j == 2
      -- second inner loop (j < 4)
      !(# u1_3, c_13 #) = L.mul_add_c# w_1 m3 u_0 c_12  -- j == 3
      -- final stanza
      !(# u_1, mc_1 #) = L.add_c# u1 c_13 mc_0
      -- end states
      -- (# l0, l0_1, l1_1, l1_2 #)
      -- (# u1_3, u_1, u2, u3 #)
      -- outer loop, i == 2 ---------------------------------------------------
      !w_2 = L.mul_w# l1_1 n
      !(# _, c_20 #) = L.mul_add_c# w_2 m0 l1_1 0##
      -- first inner loop (j < 2)
      !(# l2_1, c_21 #) = L.mul_add_c# w_2 m1 l1_2 c_20  -- j == 1
      -- second inner loop (j < 4)
      !(# u2_2, c_22 #) = L.mul_add_c# w_2 m2 u1_3 c_21  -- j == 2
      !(# u2_3, c_23 #) = L.mul_add_c# w_2 m3 u_1 c_22   -- j == 3
      -- final stanza
      !(# u_2, mc_2 #) = L.add_c# u2 c_23 mc_1
      -- end states
      -- (# l0, l0_1, l1_1, l2_1 #)
      -- (# u2_2, u2_3, u_2, u3 #)
      -- outer loop, i == 3 ---------------------------------------------------
      !w_3 = L.mul_w# l2_1 n
      !(# _, c_30 #) = L.mul_add_c# w_3 m0 l2_1 0##
      -- second inner loop (j < 4)
      !(# u3_1, c_31 #) = L.mul_add_c# w_3 m1 u2_2 c_30  -- j == 1
      !(# u3_2, c_32 #) = L.mul_add_c# w_3 m2 u2_3 c_31  -- j == 2
      !(# u3_3, c_33 #) = L.mul_add_c# w_3 m3 u_2 c_32   -- j == 3
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
  -> (# Word#, Word#, Word#, Word# #) -- ^ modulus
  -> Word#                            -- ^ mod neg inv
  -> (# Word#, Word#, Word#, Word# #) -- ^ result
redc# l u m n =
  let !(# nu, mc #) = redc_inner# u l m n
  in  WW.sub_mod_c# nu mc m m
{-# INLINE redc# #-}

redc :: Wider -> Wider -> Wider -> Word -> Wider
redc (Wider l) (Wider u) (Wider m) (W# n) =
  let !res = redc# l u m n
  in  (Wider res)

retr_inner#
  :: (# Word#, Word#, Word#, Word# #) -- ^ value in montgomery form
  -> (# Word#, Word#, Word#, Word# #) -- ^ modulus
  -> Word#                            -- ^ mod neg inv
  -> (# Word#, Word#, Word#, Word# #) -- ^ retrieved value
retr_inner# (# x0, x1, x2, x3 #) (# m0, m1, m2, m3 #) n =
  let -- outer loop, i == 0 ---------------------------------------------------
      !u_0 = L.mul_w# x0 n                              -- out state
      !(# _, o0 #) = L.mul_add_c# u_0 m0 x0 0##         -- o0, 0, 0, 0
      -- inner loop
      !(# o0_1, p0_1 #) = L.mul_add_c# u_0 m1 0## o0    -- o0_1, p0_1, 0, 0
      !(# p0_2, q0_2 #) = L.mul_add_c# u_0 m2 0## p0_1  -- o0_1, p0_2, q0_2, 0
      !(# q0_3, r0_3 #) = L.mul_add_c# u_0 m3 0## q0_2  -- o0_1, p0_2, q0_3, r0_3
      -- end state: (# o0_1, p0_2, q0_3, r0_3 #)
      -- outer loop, i == 1 ---------------------------------------------------
      !u_1 = L.mul_w# (plusWord# o0_1 x1) n
      !(# _, o1 #) = L.mul_add_c# u_1 m0 x1 o0_1        -- o1, p0_2, q0_3, r0_3
      -- inner loop
      !(# o1_1, p1_1 #) = L.mul_add_c# u_1 m1 p0_2 o1   -- o1_1, p1_1, q0_3, r0_3
      !(# p1_2, q1_2 #) = L.mul_add_c# u_1 m2 q0_3 p1_1 -- o1_1, p1_2, q1_2, r0_3
      !(# q1_3, r1_3 #) = L.mul_add_c# u_1 m3 r0_3 q1_2 -- o1_1, p1_2, q1_3, r1_3
      -- end state: (# o1_1, p1_2, q1_3, r1_3 #)
      -- outer loop, i == 2 ---------------------------------------------------
      !u_2 = L.mul_w# (plusWord# o1_1 x2) n
      !(# _, o2 #) = L.mul_add_c# u_2 m0 x2 o1_1        -- o2, p1_2, q1_3, r1_3
      -- inner loop
      !(# o2_1, p2_1 #) = L.mul_add_c# u_2 m1 p1_2 o2   -- o2_1, p2_1, q1_3, r1_3
      !(# p2_2, q2_2 #) = L.mul_add_c# u_2 m2 q1_3 p2_1 -- o2_1, p2_2, q2_2, r1_3
      !(# q2_3, r2_3 #) = L.mul_add_c# u_2 m3 r1_3 q2_2 -- o2_1, p2_2, q2_3, r2_3
      -- end state: (# o2_1, p2_2, q2_3, r2_3 #)
      -- outer loop, i == 3 ---------------------------------------------------
      !u_3 = L.mul_w# (plusWord# o2_1 x3) n
      !(# _, o3 #) = L.mul_add_c# u_3 m0 x3 o2_1        -- o3, p2_2, q2_3, r2_3
      -- inner loop
      !(# o3_1, p3_1 #) = L.mul_add_c# u_3 m1 p2_2 o3   -- o3_1, p3_1, q2_3, r2_3
      !(# p3_2, q3_2 #) = L.mul_add_c# u_3 m2 q2_3 p3_1 -- o3_1, p3_2, q3_2, r2_3
      !(# q3_3, r3_3 #) = L.mul_add_c# u_3 m3 r2_3 q3_2 -- o3_1, p3_2, q3_3, r3_3
      -- final state: (# o3_1, p3_2, q3_3, r3_3 #)
  in  (# o3_1, p3_2, q3_3, r3_3 #)
{-# INLINE retr_inner# #-}

retr#
  :: (# Word#, Word#, Word#, Word# #) -- montgomery form
  -> (# Word#, Word#, Word#, Word# #) -- modulus
  -> Word#                            -- mod neg inv
  -> (# Word#, Word#, Word#, Word# #)
retr# f m n = retr_inner# f m n
{-# INLINE retr# #-}

retr
  :: Wider -- ^ value in montgomery form
  -> Wider -- ^ modulus
  -> Word  -- ^ mod neg inv
  -> Wider -- ^ retrieved value
retr (Wider f) (Wider m) (W# n) =
  let !res = retr# f m n
  in  (Wider res)

-- | Montgomery multiplication.
mul_inner#
  :: (# Word#, Word#, Word#, Word# #)              -- ^ x
  -> (# Word#, Word#, Word#, Word# #)              -- ^ y
  -> (# Word#, Word#, Word#, Word# #)              -- ^ modulus
  -> Word#                                         -- ^ mod neg inv
  -> (# (# Word#, Word#, Word#, Word# #), Word# #) -- ^ product, meta-carry
mul_inner# (# x0, x1, x2, x3 #) (# y0, y1, y2, y3 #) (# m0, m1, m2, m3 #) n =
  let -- outer loop, i == 0 ---------------------------------------------------
      !axy0 = W.mul_w# (w x0) (w y0)                              -- out state
      !u0 = L.mul_w# (lo axy0) n                                  -- 0, 0, 0, 0
      !(# (# _, a0 #), c0 #) = W.add_c# (W.mul_w# (w u0) (w m0)) axy0
      !carry0 = (# a0, c0 #)
      -- inner loop, j == 1
      !axy0_1 = W.mul_w# (w x0) (w y1)
      !umc0_1 = W.add_w# (W.mul_w# (w u0) (w m1)) carry0
      !(# (# o0, ab0_1 #), c0_1 #) = W.add_c# axy0_1 umc0_1       -- o0, 0, 0, 0
      !carry0_1 = (# ab0_1, c0_1 #)
      -- inner loop, j == 2
      !axy0_2 = W.mul_w# (w x0) (w y2)
      !umc0_2 = W.add_w# (W.mul_w# (w u0) (w m2)) carry0_1
      !(# (# p0, ab0_2 #), c0_2 #) = W.add_c# axy0_2 umc0_2       -- o0, p0, 0, 0
      !carry0_2 = (# ab0_2, c0_2 #)
      -- inner loop, j == 3
      !axy0_3 = W.mul_w# (w x0) (w y3)
      !umc0_3 = W.add_w# (W.mul_w# (w u0) (w m3)) carry0_2
      !(# (# q0, ab0_3 #), c0_3 #) = W.add_c# axy0_3 umc0_3       -- o0, p0, q0, 0
      !carry0_3 = (# ab0_3, c0_3 #)
      -- final stanza
      !(# r0, mc0 #) = carry0_3                                   -- o0, p0, q0, r0
      -- outer loop, i == 1 ---------------------------------------------------
      !axy1 = W.add_w# (W.mul_w# (w x1) (w y0)) (w o0)
      !u1 = L.mul_w# (lo axy1) n
      !(# (# _, a1 #), c1 #) = W.add_c# (W.mul_w# (w u1) (w m0)) axy1
      !carry1 = (# a1, c1 #)
      -- inner loop, j == 1
      !axy1_1 = W.add_w# (W.mul_w# (w x1) (w y1)) (w p0)
      !umc1_1 = W.add_w# (W.mul_w# (w u1) (w m1)) carry1
      !(# (# o1, ab1_1 #), c1_1 #) = W.add_c# axy1_1 umc1_1       -- o1, p0, q0, r0
      !carry1_1 = (# ab1_1, c1_1 #)
      -- inner loop, j == 2
      !axy1_2 = W.add_w# (W.mul_w# (w x1) (w y2)) (w q0)
      !umc1_2 = W.add_w# (W.mul_w# (w u1) (w m2)) carry1_1
      !(# (# p1, ab1_2 #), c1_2 #) = W.add_c# axy1_2 umc1_2       -- o1, p1, q0, r0
      !carry1_2 = (# ab1_2, c1_2 #)
      -- inner loop, j == 3
      !axy1_3 = W.add_w# (W.mul_w# (w x1) (w y3)) (w r0)
      !umc1_3 = W.add_w# (W.mul_w# (w u1) (w m3)) carry1_2
      !(# (# q1, ab1_3 #), c1_3 #) = W.add_c# axy1_3 umc1_3       -- o1, p1, q1, r0
      !carry1_3 = (# ab1_3, c1_3 #)
      -- final stanza
      !(# r1, mc1 #) = W.add_w# carry1_3 (w mc0)                  -- o1, p1, q1, r1
      -- outer loop, i == 2 ---------------------------------------------------
      !axy2 = W.add_w# (W.mul_w# (w x2) (w y0)) (w o1)
      !u2 = L.mul_w# (lo axy2) n
      !(# (# _, a2 #), c2 #) = W.add_c# (W.mul_w# (w u2) (w m0)) axy2
      !carry2 = (# a2, c2 #)
      -- inner loop, j == 1
      !axy2_1 = W.add_w# (W.mul_w# (w x2) (w y1)) (w p1)
      !umc2_1 = W.add_w# (W.mul_w# (w u2) (w m1)) carry2
      !(# (# o2, ab2_1 #), c2_1 #) = W.add_c# axy2_1 umc2_1       -- o2, p1, q1, r1
      !carry2_1 = (# ab2_1, c2_1 #)
      -- inner loop, j == 2
      !axy2_2 = W.add_w# (W.mul_w# (w x2) (w y2)) (w q1)
      !umc2_2 = W.add_w# (W.mul_w# (w u2) (w m2)) carry2_1
      !(# (# p2, ab2_2 #), c2_2 #) = W.add_c# axy2_2 umc2_2       -- o2, p2, q1, r1
      !carry2_2 = (# ab2_2, c2_2 #)
      -- inner loop, j == 3
      !axy2_3 = W.add_w# (W.mul_w# (w x2) (w y3)) (w r1)
      !umc2_3 = W.add_w# (W.mul_w# (w u2) (w m3)) carry2_2
      !(# (# q2, ab2_3 #), c2_3 #) = W.add_c# axy2_3 umc2_3       -- o2, p2, q2, r1
      !carry2_3 = (# ab2_3, c2_3 #)
      -- final stanza
      !(# r2, mc2 #) = W.add_w# carry2_3 (w mc1)                  -- o2, p2, q2, r2
      -- outer loop, i == 3 ---------------------------------------------------
      !axy3 = W.add_w# (W.mul_w# (w x3) (w y0)) (w o2)
      !u3 = L.mul_w# (lo axy3) n
      !(# (# _, a3 #), c3 #) = W.add_c# (W.mul_w# (w u3) (w m0)) axy3
      !carry3 = (# a3, c3 #)
      -- inner loop, j == 1
      !axy3_1 = W.add_w# (W.mul_w# (w x3) (w y1)) (w p2)
      !umc3_1 = W.add_w# (W.mul_w# (w u3) (w m1)) carry3
      !(# (# o3, ab3_1 #), c3_1 #) = W.add_c# axy3_1 umc3_1       -- o3, p2, q2, r2
      !carry3_1 = (# ab3_1, c3_1 #)
      -- inner loop, j == 2
      !axy3_2 = W.add_w# (W.mul_w# (w x3) (w y2)) (w q2)
      !umc3_2 = W.add_w# (W.mul_w# (w u3) (w m2)) carry3_1
      !(# (# p3, ab3_2 #), c3_2 #) = W.add_c# axy3_2 umc3_2       -- o3, p3, q1, r2
      !carry3_2 = (# ab3_2, c3_2 #)
      -- inner loop, j == 3
      !axy3_3 = W.add_w# (W.mul_w# (w x3) (w y3)) (w r2)
      !umc3_3 = W.add_w# (W.mul_w# (w u3) (w m3)) carry3_2
      !(# (# q3, ab3_3 #), c3_3 #) = W.add_c# axy3_3 umc3_3       -- o3, p3, q3, r2
      !carry3_3 = (# ab3_3, c3_3 #)
      -- final stanza
      !(# r3, mc3 #) = W.add_w# carry3_3 (w mc2)                  -- o3, p3, q3, r3
  in  (# (# o3, p3, q3, r3 #), mc3 #)

mul
  :: Wider -- ^ lhs in montgomery form
  -> Wider -- ^ rhs in montgomery form
  -> Wider -- ^ modulus
  -> Word  -- ^ mod neg inv
  -> Wider -- ^ montgomery product
mul (Wider a) (Wider b) (Wider m) (W# n) =
  let !(# nu, mc #) = mul_inner# a b m n
  in  Wider (WW.sub_mod_c# nu mc m m)

