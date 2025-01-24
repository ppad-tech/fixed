{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Word.Extended where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits ((.|.), (.&.), (.<<.), (.>>.), (.^.))
import qualified Data.Bits as B
import qualified Data.Primitive.PrimArray as PA
import Data.Word (Word64)
import GHC.Generics
import Prelude hiding (div, mod)

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
  deriving (Eq, Show, Generic)

sel256 :: Word256 -> Int -> Word64
sel256 (Word256 a0 a1 a2 a3) = \case
  0 -> a0; 1 -> a1; 2 -> a2; 3 -> a3
  _ -> error "ppad-fixed (sel256): bad index"

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

-- utility words ------------------------------------------------------------

data Word128 = P
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Show)

data Word320 = Word320
  !Word256
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic)

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

zero576 :: Word576
zero576 = Word576 0 0 0 0 0 0 0 0 0

sel576 :: Word576 -> Int -> Word64
sel576 (Word576 a0 a1 a2 a3 a4 a5 a6 a7 a8) = \case
  0 -> a0; 1 -> a1; 2 -> a2; 3 -> a3; 4 -> a4
  5 -> a5; 6 -> a6; 7 -> a7; 8 -> a8
  _ -> error "ppad-fixed (sel576): bad index"

set576 :: Word576 -> Int -> Word64 -> Word576
set576 (Word576 a0 a1 a2 a3 a4 a5 a6 a7 a8) j wo = case j of
  0 -> Word576 wo a1 a2 a3 a4 a5 a6 a7 a8
  1 -> Word576 a0 wo a2 a3 a4 a5 a6 a7 a8
  2 -> Word576 a0 a1 wo a3 a4 a5 a6 a7 a8
  3 -> Word576 a0 a1 a2 wo a4 a5 a6 a7 a8
  4 -> Word576 a0 a1 a2 a3 wo a5 a6 a7 a8
  5 -> Word576 a0 a1 a2 a3 a4 wo a6 a7 a8
  6 -> Word576 a0 a1 a2 a3 a4 a5 wo a7 a8
  7 -> Word576 a0 a1 a2 a3 a4 a5 a6 wo a8
  8 -> Word576 a0 a1 a2 a3 a4 a5 a6 a7 wo
  _ -> error "ppad-fixed (set576): bad index"

data Word640 = Word640
  {-# UNPACK #-} !Word576
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic)

data Word832 = Word832
  {-# UNPACK #-} !Word576
  {-# UNPACK #-} !Word256
  deriving (Eq, Show, Generic)

data Word1152 = Word1152 -- yikes
  {-# UNPACK #-} !Word576
  {-# UNPACK #-} !Word576
  deriving (Eq, Show, Generic)

-- conversion -----------------------------------------------------------------

to_integer :: Word256 -> Integer
to_integer (Word256 w0 w1 w2 w3) =
      fi w3 .<<. 192
  .|. fi w2 .<<. 128
  .|. fi w1 .<<. 64
  .|. fi w0

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

lt :: Word256 -> Word256 -> Bool
lt (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(P _ c0) = sub_b a0 b0 0
      !(P _ c1) = sub_b a1 b1 c0
      !(P _ c2) = sub_b a2 b2 c1
      !(P _ c3) = sub_b a3 b3 c2
  in  c3 /= 0

gt :: Word256 -> Word256 -> Bool
gt a b = lt b a

zero :: Word256
zero = Word256 0 0 0 0

one :: Word256
one = Word256 1 0 0 0

is_zero :: Word256 -> Bool
is_zero w = w == zero

is_word64 :: Word256 -> Bool
is_word64 (Word256 _ a b c) = a == 0 && b == 0 && c == 0

-- bits -----------------------------------------------------------------------

or :: Word256 -> Word256 -> Word256
or (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  Word256 (a0 .|. b0) (a1 .|. b1) (a2 .|. b2) (a3 .|. b3)

and :: Word256 -> Word256 -> Word256
and (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  Word256 (a0 .&. b0) (a1 .&. b1) (a2 .&. b2) (a3 .&. b3)

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

-- | Addition on 'Word256' values.
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

-- XX primarray
-- x =- y * m
-- requires (len x - x_offset) >= len y > 0
sub_mul_to
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> Int
  -> PA.PrimArray Word64
  -> Word64
  -> m Word64
sub_mul_to x x_offset y m = do
  let l = PA.sizeofPrimArray y
      loop !j !borrow
        | j == l = pure borrow
        | otherwise = do
            !x_j <- PA.readPrimArray x (j + x_offset)
            let !y_j = PA.indexPrimArray y j
                !(P s carry1) = sub_b x_j borrow 0
                !(P ph pl)    = mul_c y_j m
                !(P t carry2) = sub_b s pl 0
            PA.writePrimArray x (j + x_offset) t
            loop (succ j) (ph + carry1 + carry2)
  loop 0 0

sub_mul
  :: Word576  -- dividend
  -> Int      -- min dividend index
  -> Word576  -- divisor
  -> Int      -- max divisor index
  -> Word64   -- multiplier
  -> Word640  -- result, borrow
sub_mul u u_start d d_len m = loop 0 u 0 where
  loop !j !acc !borrow
    | j == d_len = Word640 acc borrow
    | otherwise =
        let !u_j = sel576 acc (u_start + j)
            !d_j = sel576 d j
            !(P s carry1) = sub_b u_j borrow 0
            !(P ph pl)    = mul_c d_j m
            !(P t carry2) = sub_b s pl 0
            !nacc         = set576 acc (u_start + j) t
        in  loop (succ j) nacc (ph + carry1 + carry2)

-- XX primarray
-- requires (len x - x_offset) >= len y > 0
add_to
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> Int
  -> PA.PrimArray Word64
  -> m Word64
add_to x x_offset y = do
  let l = PA.sizeofPrimArray y
      loop !j !cacc
        | j == l = pure cacc
        | otherwise = do
            xj <- PA.readPrimArray x (j + x_offset)
            let yj = PA.indexPrimArray y j
                !(P nex carry) = add_c xj yj cacc
            PA.writePrimArray x (j + x_offset) nex
            loop (succ j) carry
  loop 0 0

add_big
  :: Word576
  -> Int
  -> Word576
  -> Int
  -> Word640
add_big u u_start d d_len = loop 0 u 0 where
  loop !j !acc !car
    | j == d_len = Word640 acc car
    | otherwise =
        let !u_j = sel576 acc (u_start + j)
            !d_j = sel576 d j
            !(P w64 ncar) = add_c u_j d_j car
            !nacc = set576 acc (u_start + j) w64
        in  loop (succ j) nacc ncar

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

-- XX primarray
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

quotrem_by1_gen
  :: Word576  -- dividend
  -> Int      -- dividend length
  -> Word64   -- divisor
  -> Word640
quotrem_by1_gen u ulen d =
    let !r0  = sel576 u (ulen - 1)
    in  loop (ulen - 2) zero576 r0
  where
    !rec = recip_2by1 d
    loop !j !acc !racc
      | j < 0 = Word640 acc racc
      | otherwise =
          let !u_j = sel576 u j
              !(P q_j r) = quotrem_2by1 racc u_j d rec
              !nacc = set576 acc j q_j
          in  loop (pred j) nacc r

quotrem_knuth_gen
  :: Word576
  -> Int
  -> Word576
  -> Int
  -> Word1152
quotrem_knuth_gen u ulen d dlen = loop (ulen - dlen - 1) zero576 u where
  !d_hi = sel576 d (dlen - 1)
  !d_lo = sel576 d (dlen - 2)
  !rec = recip_2by1 d_hi
  loop j !qacc !uacc
    | j < 0 = Word1152 qacc uacc
    | otherwise =
        let !u_2 = sel576 uacc (j + dlen)
            !u_1 = sel576 uacc (j + dlen - 1)
            !u_0 = sel576 uacc (j + dlen - 2)
            !qhat
              | u_2 >= d_hi = 0xffff_ffff_ffff_ffff
              | otherwise   =
                  let !(P qh rh) = quotrem_2by1 u_2 u_1 d_hi rec
                      !(P ph pl) = mul_c qh d_lo
                  in  if   ph > rh || (ph == rh && pl > u_0)
                      then qh - 1
                      else qh

            !(Word640 u0 borrow) = sub_mul u j d dlen qhat
            !u1 = set576 u0 (j + dlen) (u_2 - borrow)
        in  if   u_2 < borrow
            then
              let !qh = qhat - 1
                  !(Word640 u2 r)  = add_big u1 j d dlen
                  !u3 = set576 u2 (j + dlen) r
                  !q  = set576 qacc j qh
              in  loop (pred j) q u3
            else
              let !q = set576 qacc j qhat
              in  loop (pred j) q u1

-- XX primarray
quotrem_knuth
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> PA.MutablePrimArray (PrimState m)  Word64
  -> PA.PrimArray Word64
  -> m ()
quotrem_knuth quo u d = do
  !lu <- PA.getSizeofMutablePrimArray u
  let !ld = PA.sizeofPrimArray d
      !dh = PA.indexPrimArray d (ld - 1)
      !dl = PA.indexPrimArray d (ld - 2)
      !rec = recip_2by1 dh
      loop !j
        | j < 0 = pure ()
        | otherwise = do
            !u2 <- PA.readPrimArray u (j + ld)
            !u1 <- PA.readPrimArray u (j + ld - 1)
            !u0 <- PA.readPrimArray u (j + ld - 2)
            let !qhat | u2 >= dh  = 0xffff_ffff_ffff_ffff
                      | otherwise =
                          let !(P qh rh) = quotrem_2by1 u2 u1 dh rec
                              !(P ph pl) = mul_c qh dl
                          in  if   ph > rh || (ph == rh && pl > u0)
                              then qh - 1
                              else qh

            borrow <- sub_mul_to u j d qhat
            PA.writePrimArray u (j + ld) (u2 - borrow)
            if   u2 < borrow
            then do
              let !qh = qhat - 1
              r <- add_to u j d
              PA.writePrimArray u (j + ld) r
              PA.writePrimArray quo j qh
            else
              PA.writePrimArray quo j qhat
            loop (pred j)
  loop (lu - ld - 1)


-- XX needs work
-- quotrem_256
--   :: Word256
--   -> Word256
--   -> Word256
-- quotrem_256 u@(Word256 u0 _ _ _) d@(Word256 d0 _ _ d3) =
--     let !dlen  = hi_w d
--         !shift = B.countLeadingZeros d3
--         !(Word256 dn0 _ _ _) =
--           set (munge (dlen - 1) d zero shift) 0 (d0 .<<. shift)
--         !ulen  = hi_w u
--     in  if   ulen < dlen
--         then u
--         else
--           let !u_ulen = sel u (ulen - 1)
--               !un = set
--                 (munge
--                   (ulen - 1)
--                   u
--                   (set zero ulen (u_ulen .>>. (64 - shift)))
--                   shift)
--                 0
--                 (u0 .<<. shift)
--           in  if   dlen == 1
--               then
--                 let !(Word320 _ r) = quotrem_by1_256 un dn0
--                 in  Word256 (r .>>. shift) 0 0 0
--               else
--                 let go_r !j !acc
--                       | j == dlen = acc
--                       | otherwise =
--                           let !un_j   = sel un j
--                               !un_j_1 = sel un (j + 1)
--                               !val = (un_j  .>>. shift)
--                                  .|. (un_j_1 .<<. (64 - shift))
--                               !nacc = set acc j val
--                           in  go_r (succ j) nacc
--                     !r = go_r 0 zero
--                     !un_dlen_1 = sel un (dlen - 1)
--                 in  (set r (dlen - 1) (un_dlen_1 .>>. shift))
--   where
--     hi_w (Word256 z0 z1 z2 z3)
--       | z3 /= 0 = 4
--       | z2 /= 0 = 3
--       | z1 /= 0 = 2
--       | z0 /= 0 = 1
--       | otherwise = error "ppad-fixed (quotrem_256): division by zero"
--
--     set :: Word256 -> Int -> Word64 -> Word256
--     set w@(Word256 z0 z1 z2 z3) j val
--       | j == 0 = Word256 val z1 z2 z3
--       | j == 1 = Word256 z0 val z2 z3
--       | j == 2 = Word256 z0 z1 val z3
--       | j == 3 = Word256 z0 z1 z2 val
--       | otherwise = w
--
--     sel :: Word256 -> Int -> Word64
--     sel (Word256 z0 z1 z2 z3) j
--       | j == 0 = z0
--       | j == 1 = z1
--       | j == 2 = z2
--       | j == 3 = z3
--       | otherwise = error "ppad-fixed (select): invalid index"
--
--     munge !j !ref !acc !s
--       | j == 0 = acc
--       | otherwise =
--           let !ref_j   = sel ref j
--               !ref_j_1 = sel ref (j - 1)
--               !val  = (ref_j .<<. s) .|. (ref_j_1 .>>. (64 - s))
--               !nacc = set acc j val
--           in  munge (pred j) ref nacc s



-- quotrem_gen
--   :: Word576
--   -> Int
--   -> Word256 -- needs to be more general
--   -> Int
--   -> Word832
-- quotrem_gen u ulen d@(Word256 d0 _ _ d3) dlen =
--     let !dlen  = len_set_words d
--         !shift = B.countLeadingZeros d3
--         !ulen  = len_set_words u
--   where
--     len_set_words (Word256 z0 z1 z2 z3)
--       | z3 /= 0 = 4
--       | z2 /= 0 = 3
--       | z1 /= 0 = 2
--       | z0 /= 0 = 1
--       | otherwise = error "ppad-fixed (quotrem_256): division by zero"

-- XX primarray; dynamic size requirements
quotrem
  :: PrimMonad m
  => PA.MutablePrimArray (PrimState m) Word64
  -> PA.PrimArray Word64
  -> PA.PrimArray Word64
  -> m Word256
quotrem quo u d = do
    let !ld    = PA.sizeofPrimArray d
        !lu    = PA.sizeofPrimArray u
        !dlen  = len_loop d (ld - 1)
        !shift = B.countLeadingZeros (PA.indexPrimArray d (dlen - 1))
    dn <- PA.newPrimArray dlen
    PA.setPrimArray dn 0 dlen 0
    let go_dn !j
          | j == 0 = pure ()
          | otherwise = do
              let !dj   = PA.indexPrimArray d j
                  !dj_1 = PA.indexPrimArray d (j - 1)
                  !val  = (dj .<<. shift) .|. (dj_1 .>>. (64 - shift))
              PA.writePrimArray dn j val
              go_dn (pred j)
    go_dn (dlen - 1)
    PA.writePrimArray dn 0 (PA.indexPrimArray d 0 .<<. shift)
    let !ulen  = len_loop u (lu - 1)
    if   ulen < dlen
    then do
      let !u0 | lu >= 1   = PA.indexPrimArray u 0
              | otherwise = 0
          !u1 | lu >= 2   = PA.indexPrimArray u 1
              | otherwise = 0
          !u2 | lu >= 3   = PA.indexPrimArray u 2
              | otherwise = 0
          !u3 | lu >= 4   = PA.indexPrimArray u 3
              | otherwise = 0
      pure (Word256 u0 u1 u2 u3)
    else do
      un <- PA.newPrimArray (ulen + 1)
      PA.setPrimArray un 0 (ulen + 1) 0
      let u_ulen = PA.indexPrimArray u (ulen - 1)
      PA.writePrimArray un ulen (u_ulen .>>. (64 - shift))
      -- duplicated, but easy to handle mutableprimarrays this way
      let go_un !j
            | j == 0 = pure ()
            | otherwise = do
                let !uj   = PA.indexPrimArray u j
                    !uj_1 = PA.indexPrimArray u (j - 1)
                    !val  = (uj .<<. shift) .|. (uj_1 .>>. (64 - shift))
                PA.writePrimArray un j val
                go_un (pred j)
      go_un (ulen - 1)
      PA.writePrimArray un 0 (PA.indexPrimArray u 0 .<<. shift)
      if   dlen == 1
      then do
        dn_0 <- PA.readPrimArray dn 0
        un_c <- PA.freezePrimArray un 0 (ulen + 1)
        r <- quotrem_by1 quo un_c dn_0
        pure (Word256 (r .>>. shift) 0 0 0)
      else do
        dnf <- PA.unsafeFreezePrimArray dn
        quotrem_knuth quo un dnf
        let go_r !j !acc
              | j == dlen = pure acc
              | otherwise = do
                  un_j   <- PA.readPrimArray un j
                  un_j_1 <- PA.readPrimArray un (j + 1)
                  let !val = (un_j  .>>. shift)
                         .|. (un_j_1 .<<. (64 - shift))
                      !nacc = setr acc j val
                  go_r (succ j) nacc
        !r <- go_r 0 zero
        un_dlen_1 <- PA.readPrimArray un (dlen - 1)
        pure (setr r (dlen - 1) (un_dlen_1 .>>. shift))
  where
    len_loop !arr !j
      | j < 0 = 0
      | PA.indexPrimArray arr j /= 0 = j + 1
      | otherwise = len_loop arr (pred j)

    setr w@(Word256 z0 z1 z2 z3) j val
      | j == 0 = Word256 val z1 z2 z3
      | j == 1 = Word256 z0 val z2 z3
      | j == 2 = Word256 z0 z1 val z3
      | j == 3 = Word256 z0 z1 z2 val
      | otherwise = w

-- primarray
div :: Word256 -> Word256 -> Word256
div a@(Word256 a0 a1 a2 a3) b@(Word256 b0 b1 b2 b3)
  | is_zero b || b `gt` a = zero -- ?
  | a == b                = one
  | is_word64 a           = Word256 (a0 `quot` b0) 0 0 0
  | otherwise = runST $ do
      quo <- PA.newPrimArray 4
      PA.setPrimArray quo 0 4 0
      mx <- PA.newPrimArray 4
      my <- PA.newPrimArray 4
      PA.writePrimArray mx 0 a0
      PA.writePrimArray mx 1 a1
      PA.writePrimArray mx 2 a2
      PA.writePrimArray mx 3 a3
      PA.writePrimArray my 0 b0
      PA.writePrimArray my 1 b1
      PA.writePrimArray my 2 b2
      PA.writePrimArray my 3 b3
      x <- PA.unsafeFreezePrimArray mx
      y <- PA.unsafeFreezePrimArray my
      _ <- quotrem quo x y
      z0 <- PA.readPrimArray quo 0
      z1 <- PA.readPrimArray quo 1
      z2 <- PA.readPrimArray quo 2
      z3 <- PA.readPrimArray quo 3
      pure (Word256 z0 z1 z2 z3)

-- primarray
mod :: Word256 -> Word256 -> Word256
mod a@(Word256 a0 a1 a2 a3) b@(Word256 b0 b1 b2 b3)
  | is_zero b || a == b = zero -- ?
  | a `lt` b = a
  | is_word64 a = Word256 (a0 `Prelude.rem` b0) 0 0 0
  | otherwise = runST $ do
      quo <- PA.newPrimArray 4
      PA.setPrimArray quo 0 4 0
      mx <- PA.newPrimArray 4
      my <- PA.newPrimArray 4
      PA.writePrimArray mx 0 a0
      PA.writePrimArray mx 1 a1
      PA.writePrimArray mx 2 a2
      PA.writePrimArray mx 3 a3
      PA.writePrimArray my 0 b0
      PA.writePrimArray my 1 b1
      PA.writePrimArray my 2 b2
      PA.writePrimArray my 3 b3
      x <- PA.unsafeFreezePrimArray mx
      y <- PA.unsafeFreezePrimArray my
      quotrem quo x y

