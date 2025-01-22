{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
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


