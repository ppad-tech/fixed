{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.Word.Extended
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Large fixed-width words, complete with support for conversion,
-- comparison, bitwise operations, arithmetic, and modular arithmetic.

module Data.Word.Extended (
    Word256(..)
  , zero
  , one

  -- * Conversion
  , to_integer
  , to_word256

  -- * Comparison
  , lt
  , gt
  , is_zero

  -- * Bit Operations
  , or
  , and
  , xor

  -- * Arithmetic
  , add
  , sub
  , mul
  , div

  -- * Modular Arithmetic
  , mod

  -- for testing/benchmarking
  , Word128(..)
  , quotrem
  , quotrem_r
  , quotrem_by1
  , quotrem_2by1
  , quotrem_knuth
  , recip_2by1
  , to_word512
  , word512_to_integer
  , mul_512
  , mul_c
  , umul_hop
  , umul_step
  ) where

import Control.DeepSeq
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.), (.^.))
import qualified Data.Bits as B
import qualified Data.Primitive.PrimArray as PA
import Data.Word (Word64)
import GHC.Generics
import Prelude hiding (div, mod, or, and)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- word256, word512 -----------------------------------------------------------

-- | Little-endian Word256.
data Word256 = Word256
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Generic)

instance NFData Word256

instance Show Word256 where
  show = show . to_integer

-- | Little-endian Word512.
data Word512 = Word512
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic)

instance NFData Word512

-- utility words ------------------------------------------------------------

data Word128 = P
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic)

instance NFData Word128

data Word320 = Word320
  !Word256
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic)

instance NFData Word320

data Word576 = Word576
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic)

instance NFData Word576

data Word640 = Word640
  {-# UNPACK #-} !Word576
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic)

instance NFData Word640

data Word832 = Word832
  {-# UNPACK #-} !Word576
  {-# UNPACK #-} !Word256
  deriving (Eq, Show, Generic)

instance NFData Word832

data Word1152 = Word1152
  {-# UNPACK #-} !Word576
  {-# UNPACK #-} !Word576
  deriving (Eq, Show, Generic)

instance NFData Word1152

-- conversion -----------------------------------------------------------------

-- | Convert a fixed-width 'Word256' into a variable-length 'Integer'.
--
--   >>> let foo = to_integer (Word256 0x1 0x10 0x100 0x1000)
--   >>> foo
--   25711008708143844408758505763390361887002166947932397379780609
to_integer :: Word256 -> Integer
to_integer (Word256 w0 w1 w2 w3) =
      fi w3 .<<. 192
  .|. fi w2 .<<. 128
  .|. fi w1 .<<. 64
  .|. fi w0

-- | Convert a fixed-width 'Word256' into a variable-length 'Integer'.
--
--   >>> (\(Word256 l _ _ _) -> l) (to_word256 foo)
--   1
to_word256 :: Integer -> Word256
to_word256 n =
  let !mask64 = 2 ^ (64 :: Int) - 1
      !w0 = fi (n .&. mask64)
      !w1 = fi ((n .>>. 64) .&. mask64)
      !w2 = fi ((n .>>. 128) .&. mask64)
      !w3 = fi ((n .>>. 192) .&. mask64)
  in  Word256 w0 w1 w2 w3

word512_to_integer :: Word512 -> Integer
word512_to_integer (Word512 w0 w1 w2 w3 w4 w5 w6 w7) =
      fi w7 .<<. 448
  .|. fi w6 .<<. 384
  .|. fi w5 .<<. 320
  .|. fi w4 .<<. 256
  .|. fi w3 .<<. 192
  .|. fi w2 .<<. 128
  .|. fi w1 .<<. 64
  .|. fi w0

to_word512 :: Integer -> Word512
to_word512 n =
  let !mask64 = 2 ^ (64 :: Int) - 1
      !w0 = fi (n .&. mask64)
      !w1 = fi ((n .>>. 64) .&. mask64)
      !w2 = fi ((n .>>. 128) .&. mask64)
      !w3 = fi ((n .>>. 192) .&. mask64)
      !w4 = fi ((n .>>. 256) .&. mask64)
      !w5 = fi ((n .>>. 320) .&. mask64)
      !w6 = fi ((n .>>. 384) .&. mask64)
      !w7 = fi ((n .>>. 448) .&. mask64)
  in  Word512 w0 w1 w2 w3 w4 w5 w6 w7

-- comparison -----------------------------------------------------------------

-- | Strict less-than comparison on 'Word256' values.
--
--   >>> to_word256 0 `lt` to_word256 1
--   True
--   >>> to_word256 0 `lt` to_word256 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
--   True
lt :: Word256 -> Word256 -> Bool
lt (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(P _ c0) = sub_b a0 b0 0
      !(P _ c1) = sub_b a1 b1 c0
      !(P _ c2) = sub_b a2 b2 c1
      !(P _ c3) = sub_b a3 b3 c2
  in  c3 /= 0

-- | Strict greater-than comparison on 'Word256' values.
--
--   >>> to_word256 0 `gt` to_word256 1
--   False
--   >>> to_word256 0 `gt` to_word256 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
--   False
gt :: Word256 -> Word256 -> Bool
gt a b = lt b a

-- | Zero, as a 'Word256'.
zero :: Word256
zero = Word256 0 0 0 0

-- | One, as a 'Word256'.
one :: Word256
one = Word256 1 0 0 0

-- | Test if a 'Word256' value is zero.
is_zero :: Word256 -> Bool
is_zero w = w == zero

is_word64 :: Word256 -> Bool
is_word64 (Word256 _ a b c) = a == 0 && b == 0 && c == 0

-- bits -----------------------------------------------------------------------

-- | Bitwise-or on 'Word256' values.
or :: Word256 -> Word256 -> Word256
or (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  Word256 (a0 .|. b0) (a1 .|. b1) (a2 .|. b2) (a3 .|. b3)

-- | Bitwise-and on 'Word256' values.
and :: Word256 -> Word256 -> Word256
and (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  Word256 (a0 .&. b0) (a1 .&. b1) (a2 .&. b2) (a3 .&. b3)

-- | Bitwise-xor on 'Word256' values.
xor :: Word256 -> Word256 -> Word256
xor (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  Word256 (a0 .^. b0) (a1 .^. b1) (a2 .^. b2) (a3 .^. b3)

-- addition, subtraction ------------------------------------------------------

-- add-with-carry
--
-- x86-64 ADDQ rX, rY
--        ADCQ $0, rCarry
--
-- ARM    ADDS
--        ADC
add_c :: Word64 -> Word64 -> Word64 -> Word128
add_c w64_0 w64_1 c =
  let !s = w64_0 + w64_1 + c
      !n | s < w64_0 || s < w64_1 = 1
         | otherwise = 0
  in  P s n

-- addition with overflow indication
add_of :: Word256 -> Word256 -> Word320
add_of (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(P s0 c0) = add_c a0 b0 0
      !(P s1 c1) = add_c a1 b1 c0
      !(P s2 c2) = add_c a2 b2 c1
      !(P s3 c3) = add_c a3 b3 c2
  in  Word320 (Word256 s0 s1 s2 s3) c3

-- | Addition on 'Word256' values, with overflow.
--
--   >>> to_word256 0xFFFFFFFFFF `add` to_word256 0xFFFFFF
--   18446742974181146625
add :: Word256 -> Word256 -> Word256
add w0 w1 = s where
  !(Word320 s _) = add_of w0 w1

-- subtract-with-borrow
--
-- x86-64  SUBQ rY, rX
--         SBBQ $0, rBorrow
--
-- ARM     SUBS
--         SBC
sub_b :: Word64 -> Word64 -> Word64 -> Word128
sub_b w64_0 w64_1 b =
  let !d = w64_0 - w64_1 - b
      !n | w64_0 < w64_1 + b = 1
         | otherwise = 0
  in  P d n

sub_of :: Word256 -> Word256 -> Word320
sub_of (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(P s0 c0) = sub_b a0 b0 0
      !(P s1 c1) = sub_b a1 b1 c0
      !(P s2 c2) = sub_b a2 b2 c1
      !(P s3 c3) = sub_b a3 b3 c2
  in  Word320 (Word256 s0 s1 s2 s3) c3

-- | Subtraction on 'Word256' values.
--
--   >>> to_word256 0xFFFFFFFFFF `sub` to_word256 0xFFFFFF
--   1099494850560
sub :: Word256 -> Word256 -> Word256
sub w0 w1 = d where
  !(Word320 d _) = sub_of w0 w1

-- multiplication -------------------------------------------------------------

-- x86-64 (BMI2)    MULX
--        (RDX:RAX) MULQ
--
-- ARM    UMULH
--
-- translated from Mul64 in go's math/bits package
mul_c :: Word64 -> Word64 -> Word128
mul_c x y =
  let !mask32 = 0xffffffff
      !x0 = x .&. mask32
      !y0 = y .&. mask32
      !x1 = x .>>. 32
      !y1 = y .>>. 32

      !w0   = x0 * y0
      !t    = x1 * y0 + w0 .>>. 32
      !w1   = t .&. mask32
      !w2   = t .>>. 32
      !w1_1 = w1 + x0 * y1

      !hi = x1 * y1 + w2 + w1_1 .>>. 32
      !lo = x * y
  in  P hi lo

-- (hi * 2 ^ 64 + lo) = z + (x * y)
umul_hop :: Word64 -> Word64 -> Word64 -> Word128
umul_hop z x y =
  let !(P hi_0 lo_0) = mul_c x y
      !(P lo c)      = add_c lo_0 z 0
      !(P hi _)      = add_c hi_0 0 c
  in  P hi lo

-- (hi * 2 ^ 64 + lo) = z + (x * y) + c
umul_step :: Word64 -> Word64 -> Word64 -> Word64 -> Word128
umul_step z x y c =
  let !(P hi_0 lo_0) = mul_c x y
      !(P lo_1 c_0)  = add_c lo_0 c 0
      !(P hi_1 _)    = add_c hi_0 0 c_0
      !(P lo c_1)    = add_c lo_1 z 0
      !(P hi _)      = add_c hi_1 0 c_1
  in  P hi lo

-- | Multiplication on 'Word256' values, with overflow.
--
--   >>> to_word256 0xFFFFFFFFFF `mul` to_word256 0xFFFFFF
--   18446742974181146625
mul :: Word256 -> Word256 -> Word256
mul (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(P c0_0 s0) = mul_c a0 b0
      !(P c0_1 r0) = umul_hop c0_0 a1 b0
      !(P c0_2 r1) = umul_hop c0_1 a2 b0

      !(P c1_0 s1) = umul_hop r0 a0 b1
      !(P c1_1 r2) = umul_step r1 a1 b1 c1_0

      !(P c2 s2)   = umul_hop r2 a1 b1

      !s3 = a3 * b0 + a2 * b1 + a0 * b3 + a1 * b2 + c0_2 + c1_1 + c2
  in  Word256 s0 s1 s2 s3

-- | Multiplication on 'Word256' values, to 'Word512'.
mul_512 :: Word256 -> Word256 -> Word512
mul_512 (Word256 x0 x1 x2 x3) (Word256 y0 y1 y2 y3) =
  let !(P c4_0   r0) = mul_c x0 y0
      !(P c4_1 r0_1) = umul_hop c4_0 x1 y0
      !(P c4_2 r0_2) = umul_hop c4_1 x2 y0
      !(P c4   r0_3) = umul_hop c4_2 x3 y0

      !(P c5_0   r1) = umul_hop r0_1 x0 y1
      !(P c5_1 r1_2) = umul_step r0_2 x1 y1 c5_0
      !(P c5_2 r1_3) = umul_step r0_3 x2 y1 c5_1
      !(P c5   r1_4) = umul_step c4 x3 y1 c5_2

      !(P c6_0   r2) = umul_hop r1_2 x0 y2
      !(P c6_1 r2_3) = umul_step r1_3 x1 y2 c6_0
      !(P c6_2 r2_4) = umul_step r1_4 x2 y2 c6_1
      !(P c6   r2_5) = umul_step c5 x3 y2 c6_2

      !(P c7_0   r3) = umul_hop r2_3 x0 y3
      !(P c7_1   r4) = umul_step r2_4 x1 y3 c7_0
      !(P c7_2   r5) = umul_step r2_5 x2 y3 c7_1
      !(P r7     r6) = umul_step c6 x3 y3 c7_2
  in  Word512 r0 r1 r2 r3 r4 r5 r6 r7

-- division -------------------------------------------------------------------

-- primitive -------------------------

-- x =- y * m
-- requires (len x - x_offset) >= len y > 0
sub_mul
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> Int
  -> Word256
  -> Int
  -> Word64
  -> m Word64
sub_mul x x_offset (Word256 y0 y1 y2 y3) l m = do
  let loop !j !borrow
        | j == l = pure borrow
        | otherwise = do
            !x_j <- PA.readPrimArray x (j + x_offset)
            let !(P s carry1) = sub_b x_j borrow 0
                !(P ph pl)    = case j of
                  0 -> mul_c y0 m
                  1 -> mul_c y1 m
                  2 -> mul_c y2 m
                  3 -> mul_c y3 m
                  _ -> error "ppad-fixed (sub_mul): bad index"
                !(P t carry2) = sub_b s pl 0
            PA.writePrimArray x (j + x_offset) t
            loop (succ j) (ph + carry1 + carry2)
  loop 0 0

quotrem_by1
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> PA.PrimArray Word64
  -> Word64
  -> m Word64
quotrem_by1 quo u d = do
  let !rec = recip_2by1 d
      !lu  = PA.sizeofPrimArray u
      !r0  = PA.indexPrimArray u (lu - 1)
      loop !j !racc
        | j < 0 = pure racc
        | otherwise = do
            let uj = PA.indexPrimArray u j
                !(P qj rnex) = quotrem_2by1 racc uj d rec
            PA.writePrimArray quo j qj
            loop (pred j) rnex
  loop (lu - 2) r0

add_to
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> Int
  -> Word256
  -> Int
  -> m Word64
add_to x x_offset (Word256 y0 y1 y2 y3) l = do
  let loop !j !cacc
        | j == l = pure cacc
        | otherwise = do
            xj <- PA.readPrimArray x (j + x_offset)
            let !(P nex carry) = case j of
                  0 -> add_c xj y0 cacc
                  1 -> add_c xj y1 cacc
                  2 -> add_c xj y2 cacc
                  3 -> add_c xj y3 cacc
                  _ -> error "ppad-fixed (add_to): bad index"
            PA.writePrimArray x (j + x_offset) nex
            loop (succ j) carry
  loop 0 0

quotrem
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64 -- quotient  (potentially large)
  -> PA.PrimArray Word64                      -- dividend  (potentially large)
  -> Word256                                  -- divisor   (256-bit)
  -> m Word256                                -- remainder (256-bit)
quotrem quo u (Word256 d0 d1 d2 d3) = do
  let -- normalize divisor
      (dlen, shift)
        | d3 /= 0   = (4, B.countLeadingZeros d3)
        | d2 /= 0   = (3, B.countLeadingZeros d2)
        | d1 /= 0   = (2, B.countLeadingZeros d1)
        | otherwise = (1, B.countLeadingZeros d0) -- zero not checked
      dn_3 = (d3 .<<. shift) .|. (d2 .>>. (64 - shift))
      dn_2 = (d2 .<<. shift) .|. (d1 .>>. (64 - shift))
      dn_1 = (d1 .<<. shift) .|. (d0 .>>. (64 - shift))
      dn_0 = d0 .<<. shift
      !dn = Word256 dn_0 dn_1 dn_2 dn_3
      -- get size of normalized dividend
      lu = PA.sizeofPrimArray u
      ulen = let loop !j
                   | j < 0 = 0
                   | PA.indexPrimArray u j /= 0 = j + 1
                   | otherwise = loop (j - 1)
             in  loop (lu - 1)
  if   ulen < dlen
  then do
    -- u always has size at least 4
    let u0 = PA.indexPrimArray u 0; u1 = PA.indexPrimArray u 1
        u2 = PA.indexPrimArray u 2; u3 = PA.indexPrimArray u 3
    pure (Word256 u0 u1 u2 u3)
  else do
    -- normalize dividend
    !un <- PA.newPrimArray (ulen + 1)
    PA.setPrimArray un 0 (ulen + 1) 0
    let u_hi = PA.indexPrimArray u (ulen - 1)
    PA.writePrimArray un ulen (u_hi .>>. (64 - shift))
    let normalize_u !j !uj
          | j == 0 =
              PA.writePrimArray un 0 (PA.indexPrimArray u 0 .<<. shift)
          | otherwise = do
              let !uj_1 = PA.indexPrimArray u (j - 1)
                  !val  = (uj .<<. shift) .|. (uj_1 .>>. (64 - shift))
              PA.writePrimArray un j val
              normalize_u (pred j) uj_1
    normalize_u (ulen - 1) u_hi
    if   dlen == 1
    then do
     -- normalized divisor is small
      !un_final <- PA.unsafeFreezePrimArray un
      !r <- quotrem_by1 quo un_final dn_0
      pure (Word256 (r .>>. shift) 0 0 0)
    else do
      quotrem_knuth quo un dn dlen
      -- compute unnormalized remainder
      let unn_rem !j !(Word256 r0 r1 r2 r3) !un_j
            | j == dlen = pure $ case j of
                2 -> Word256 r0 (un_j .>>. shift) r2 r3
                3 -> Word256 r0 r1 (un_j .>>. shift) r3
                4 -> Word256 r0 r1 r2 (un_j .>>. shift)
                _ -> error "ppad-fixed (quotrem): bad index"
            | otherwise = do
                un_j_1 <- PA.readPrimArray un (j + 1)
                let !unn_j = (un_j .>>. shift) .|. (un_j_1 .<<. (64 - shift))
                    !nacc = case j of
                      0 -> Word256 unn_j 0 0 0
                      1 -> Word256 r0 unn_j 0 0
                      2 -> Word256 r0 r1 unn_j 0
                      3 -> Word256 r0 r1 r2 unn_j
                      _ -> error "ppad-fixed (quotrem): bad index"
                unn_rem (j + 1) nacc un_j_1
      !un_0 <- PA.readPrimArray un 0
      unn_rem 0 zero un_0

quotrem_knuth
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64  -- quotient (potentially large)
  -> PA.MutablePrimArray (PrimState m)  Word64 -- normalized dividend
  -> Word256                                   -- normalized divisor
  -> Int                                       -- words in normalized divisor
  -> m ()
quotrem_knuth quo u d@(Word256 d0 d1 d2 d3) ld = do
  !lu <- PA.getSizeofMutablePrimArray u
  let (dh, dl) = case ld of
        4 -> (d3, d2)
        3 -> (d2, d1)
        2 -> (d1, d0)
        _ -> error "ppad-fixed (quotrem_knuth): bad index"
      !rec = recip_2by1 dh
      loop !j
        | j < 0 = pure ()
        | otherwise = do
            !u2 <- PA.readPrimArray u (j + ld)
            !u1 <- PA.readPrimArray u (j + ld - 1)
            !u0 <- PA.readPrimArray u (j + ld - 2)
            let !qhat
                  | u2 >= dh  = 0xffff_ffff_ffff_ffff
                  | otherwise =
                      let !(P qh rh) = quotrem_2by1 u2 u1 dh rec
                          !(P ph pl) = mul_c qh dl
                      in  if   ph > rh || (ph == rh && pl > u0)
                          then qh - 1
                          else qh

            borrow <- sub_mul u j d ld qhat
            PA.writePrimArray u (j + ld) (u2 - borrow)
            if   u2 < borrow
            then do
              let !qh = qhat - 1
              r <- add_to u j d ld
              PA.writePrimArray u (j + ld) r
              PA.writePrimArray quo j qh
            else
              PA.writePrimArray quo j qhat
            loop (pred j)
  loop (lu - ld - 1)

-- quotient, remainder of (hi, lo) divided by y
-- translated from Div64 in go's math/bits package
--
-- x86-64 (RDX:RAX)  DIVQ
quotrem_r :: Word64 -> Word64 -> Word64 -> Word128
quotrem_r hi lo y_0
    | y_0 == 0  = error "ppad-fixed: division by zero"
    | y_0 <= hi = error "ppad-fixed: overflow"
    | hi == 0   = P (lo `quot` y_0) (lo `rem` y_0)
    | otherwise =
        let !s = B.countLeadingZeros y_0
            !y = y_0 .<<. s

            !yn1  = y .>>. 32
            !yn0  = y .&. mask32
            !un32 = (hi .<<. s) .|. (lo .>>. (64 - s))
            !un10 = lo .<<. s
            !un1 = un10 .>>. 32
            !un0 = un10 .&. mask32
            !q1 = un32 `quot` yn1
            !rhat = un32 - q1 * yn1

            !q1_l = q_loop q1 rhat yn0 yn1 un1

            !un21 = un32 * two32 + un1 - q1_l * y
            !q0 = un21 `quot` yn1
            !rhat_n = un21 - q0 * yn1

            !q0_l = q_loop q0 rhat_n yn0 yn1 un0
        in  P
              (q1_l * two32 + q0_l)
              ((un21 * two32 + un0 - q0_l * y) .>>. s)
  where
    !two32  = 0x100000000
    !mask32 = 0x0ffffffff

    q_loop !q_acc !rhat_acc !yn_0 !yn_1 !un =
      let go !qa !rha
            | qa >= two32 || qa * yn_0 > two32 * rha + un =
                let !qn = qa - 1
                    !rhn = rha + yn_1
                in  if   rhn >= two32
                    then qn
                    else go qn rhn
            | otherwise = qa
      in  go q_acc rhat_acc

recip_2by1 :: Word64 -> Word64
recip_2by1 d = r where
  !(P r _) = quotrem_r (B.complement d) 0xffffffffffffffff d

quotrem_2by1 :: Word64 -> Word64 -> Word64 -> Word64 -> Word128
quotrem_2by1 uh ul d rec =
  let !(P qh_0 ql) = mul_c rec uh
      !(P ql_0 c)  = add_c ql ul 0
      !(P (succ -> qh_1) _)  = add_c qh_0 uh c
      !r = ul - qh_1 * d

      !(P qh_y r_y)
        | r > ql_0  = P (qh_1 - 1) (r + d)
        | otherwise = P qh_1 r

  in  if   r_y >= d
      then P (qh_y + 1) (r_y - d)
      else P qh_y r_y


div :: Word256 -> Word256 -> Word256
div u@(Word256 u0 u1 u2 u3) d@(Word256 d0 _ _ _)
  | is_zero d || d `gt` u = zero -- ?
  | u == d                = one
  | is_word64 u           = Word256 (u0 `quot` d0) 0 0 0
  | otherwise = runST $ do
      -- allocate quotient
      quo <- PA.newPrimArray 4
      PA.setPrimArray quo 0 4 0
      -- allocate dividend
      u_arr <- PA.newPrimArray 4
      PA.setPrimArray u_arr 0 4 0
      PA.writePrimArray u_arr 0 u0
      PA.writePrimArray u_arr 1 u1
      PA.writePrimArray u_arr 2 u2
      PA.writePrimArray u_arr 3 u3
      u_final <- PA.unsafeFreezePrimArray u_arr
      _ <- quotrem quo u_final d
      q0 <- PA.readPrimArray quo 0
      q1 <- PA.readPrimArray quo 1
      q2 <- PA.readPrimArray quo 2
      q3 <- PA.readPrimArray quo 3
      pure (Word256 q0 q1 q2 q3)

-- | Modulo operation on 'Word256' values.
--
--   >>> to_word256 0xFFFFFFFFFF `mod` to_word256 0xFFFFFF
--   65535
mod :: Word256 -> Word256 -> Word256
mod u@(Word256 u0 u1 u2 u3) d@(Word256 d0 _ _ _)
  | is_zero d || d `gt` u = zero -- ?
  | u == d                = one
  | is_word64 u           = Word256 (u0 `quot` d0) 0 0 0
  | otherwise = runST $ do
      -- allocate quotient
      quo <- PA.newPrimArray 4
      PA.setPrimArray quo 0 4 0
      -- allocate dividend
      u_arr <- PA.newPrimArray 4
      PA.setPrimArray u_arr 0 4 0
      PA.writePrimArray u_arr 0 u0
      PA.writePrimArray u_arr 1 u1
      PA.writePrimArray u_arr 2 u2
      PA.writePrimArray u_arr 3 u3
      u_final <- PA.unsafeFreezePrimArray u_arr
      quotrem quo u_final d
