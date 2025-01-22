{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}

module Data.Word.Extended where

import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import Data.Word (Word64)
import GHC.Generics

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- word256 --------------------------------------------------------------------

-- | Little-endian Word256.
data Word256 = Word256
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
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

-- addition, subtraction ------------------------------------------------------

-- strict, unboxed pair of Word64
data W64Pair = W64P
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
  deriving (Eq, Show)

-- add-with-carry
add_c :: Word64 -> Word64 -> Word64 -> W64Pair
add_c w64_0 w64_1 c =
  let !s = w64_0 + w64_1 + c
      !n | s < w64_0 || s < w64_1 = 1
         | otherwise = 0
  in  W64P s n

data Word256WithOverflow = Word256WithOverflow
  !Word256
  {-# UNPACK #-} !Bool
  deriving (Eq, Show)

-- addition with overflow indication
add_of :: Word256 -> Word256 -> Word256WithOverflow
add_of (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(W64P s0 c0) = add_c a0 b0 0
      !(W64P s1 c1) = add_c a1 b1 c0
      !(W64P s2 c2) = add_c a2 b2 c1
      !(W64P s3 c3) = add_c a3 b3 c2
  in  Word256WithOverflow
        (Word256 s0 s1 s2 s3)
        (c3 /= 0)

-- | Addition on 'Word256' values.
add :: Word256 -> Word256 -> Word256
add w0 w1 = s where
  !(Word256WithOverflow s _) = add_of w0 w1

-- subtract-with-borrow
sub_b :: Word64 -> Word64 -> Word64 -> W64Pair
sub_b w64_0 w64_1 b =
  let !d = w64_0 - w64_1 - b
      !n | w64_0 < w64_1 + b = 1
         | otherwise = 0
  in  W64P d n

sub_of :: Word256 -> Word256 -> Word256WithOverflow
sub_of (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(W64P s0 c0) = sub_b a0 b0 0
      !(W64P s1 c1) = sub_b a1 b1 c0
      !(W64P s2 c2) = sub_b a2 b2 c1
      !(W64P s3 c3) = sub_b a3 b3 c2
  in  Word256WithOverflow
        (Word256 s0 s1 s2 s3)
        (c3 /= 0)

-- | Subtraction on 'Word256' values.
sub :: Word256 -> Word256 -> Word256
sub w0 w1 = d where
  !(Word256WithOverflow d _) = sub_of w0 w1

-- multiplication -------------------------------------------------------------

-- note that this is available in a single MULX instruction on e.g.
-- x86_64 with BMI2
--
-- translated from Mul64 in go's math/bits package
mul_c :: Word64 -> Word64 -> W64Pair
mul_c x y =
  let !mask32 = 0xffffffff
      !x0 = x .&. mask32
      !x1 = x .>>. 32
      !y0 = y .&. mask32
      !y1 = y .>>. 32

      !w0   = x0 * y0
      !t    = x1 * y0 + w0 .>>. 32
      !w1   = t .&. mask32
      !w2   = t .>>. 32
      !w1_1 = w1 + x0 * y1

      !hi = x1 * y1 + w2 + w1_1 .>>. 32
      !lo = x * y
  in  W64P hi lo

-- (hi * 2 ^ 64 + lo) = z + (x * y)
umul_hop :: Word64 -> Word64 -> Word64 -> W64Pair
umul_hop z x y =
  let !(W64P hi_0 lo_0) = mul_c x y
      !(W64P lo c)      = add_c lo_0 z 0
      !(W64P hi _)      = add_c hi_0 0 c
  in  W64P hi lo

-- (hi * 2 ^ 64 + lo) = z + (x * y) + c
umul_step :: Word64 -> Word64 -> Word64 -> Word64 -> W64Pair
umul_step z x y c =
  let !(W64P hi_0 lo_0) = mul_c x y
      !(W64P lo_1 c_0)  = add_c lo_0 c 0
      !(W64P hi_1 _)    = add_c hi_0 0 c_0
      !(W64P lo c_1)    = add_c lo_1 z 0
      !(W64P hi _)      = add_c hi_1 0 c_1
  in  W64P hi lo

mul :: Word256 -> Word256 -> Word256
mul (Word256 a0 a1 a2 a3) (Word256 b0 b1 b2 b3) =
  let !(W64P c0_0 s0) = mul_c a0 b0
      !(W64P c0_1 r0) = umul_hop c0_0 a1 b0
      !(W64P c0_2 r1) = umul_hop c0_1 a2 b0

      !(W64P c1_0 s1) = umul_hop r0 a0 b1
      !(W64P c1_1 r2) = umul_step r1 a1 b1 c1_0

      !(W64P c2 s2)   = umul_hop r2 a1 b1

      !s3 = a3 * b0 + a2 * b1 + a0 * b3 + a1 * b2 + c0_2 + c1_1 + c2
  in  Word256 s0 s1 s2 s3

