{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- XX this should probably be its own library

module Data.Word.Montgomery where

import Control.DeepSeq
import qualified Data.Choice as C
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import qualified Data.Bits as B
import qualified Data.Word.Limb as L
import qualified Data.Word.Wider as L
import GHC.Exts
import Prelude hiding (div, mod, or, and, not, quot, rem, recip)

-- XX my eyes, it burns

redc_inner#
  :: (# Word#, Word#, Word#, Word# #)     -- upper
  -> (# Word#, Word#, Word#, Word# #)     -- lower
  -> (# Word#, Word#, Word#, Word# #)     -- modulus
  -> Word#
  -> (# (# Word#, Word#, Word#, Word# #), Word# #) -- upper, meta-carry
redc_inner#
    (# u0, u1, u2, u3 #)
    (# l0, l1, l2, l3 #)
    (# m0, m1, m2, m3 #)
    mninv =
  let -- outer loop, i == 0 ---------------------------------------------------
      !w_0 = L.mul_w# l0 mninv
      !(# _, c_00 #) = L.mul_add_c# w_0 m0 l0 0##   -- m0, l0
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
      !w_1 = L.mul_w# l0_1 mninv
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
      !w_2 = L.mul_w# l1_1 mninv
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
      !w_3 = L.mul_w# l2_1 mninv
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

