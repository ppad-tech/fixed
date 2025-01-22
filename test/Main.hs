{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits ((.|.), (.&.), (.<<.), (.>>.))
import Data.Word (Word64)
import Data.Word.Extended
import Test.Tasty
import qualified Test.Tasty.QuickCheck as Q

instance Q.Arbitrary Word256 where
  arbitrary = do
    w0 <- Q.arbitrary
    w1 <- Q.arbitrary
    w2 <- Q.arbitrary
    w3 <- Q.arbitrary
    pure (Word256 w0 w1 w2 w3)

-- second argument is no greater than first argument
newtype Monotonic a = Monotonic (a, a)
  deriving Show

instance (Q.Arbitrary a, Ord a) => Q.Arbitrary (Monotonic a) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary `Q.suchThat` (\b -> b <= a)
    pure (Monotonic (a, b))

-- second argument * third argument is no greater than first argument
newtype MulMonotonic = MulMonotonic (Integer, Integer, Integer)
  deriving Show

instance Q.Arbitrary MulMonotonic where
  arbitrary = do
    Q.NonNegative a <- Q.arbitrary :: Q.Gen (Q.NonNegative Integer)
    m <- fmap fi (Q.arbitrary :: Q.Gen Word64)
    Q.NonNegative b <-
      fmap Q.NonNegative (Q.arbitrary `Q.suchThat` (\b -> b * m <= a))
    pure (MulMonotonic (a, b, m))

-- properties -----------------------------------------------------------------

mul_c_matches :: Word64 -> Word64 -> Bool
mul_c_matches a b =
  let c = fi a * fi b :: Integer
      c_hi = fi (c .>>. 64) :: Word64
      c_lo = fi (c .&. 0xffffffffffffffff) :: Word64

      P hi lo = mul_c a b
  in  hi == c_hi && lo == c_lo

-- (hi * 2 ^ 64 + lo) = z + (x * y)
umul_hop_predicate_holds :: Word64 -> Word64 -> Word64 -> Bool
umul_hop_predicate_holds z x y =
  let !(P hi lo) = umul_hop z x y
  in  fi hi * 2 ^ (64 :: Int) + fi lo == (fi z + (fi x * fi y) :: Integer)

-- (hi * 2 ^ 64 + lo) = z + (x * y) + c
umul_step_predicate_holds :: Word64 -> Word64 -> Word64 -> Word64 -> Bool
umul_step_predicate_holds z x y c =
  let !(P hi lo) = umul_step z x y c
      !left = fi hi * 2 ^ (64 :: Int) + fi lo :: Integer
      !rite = fi z + (fi x * fi y) + fi c :: Integer
  in  left == rite

sub_mul_matches :: MulMonotonic -> Bool
sub_mul_matches (MulMonotonic (x, y, m)) =
    let !left = to_word256 (x - y * m)
        !(Word256WithOverflow rite _)
          = sub_mul (to_word256 x) (to_word256 y) (fi m)
    in  left == rite

to_word256_inverts_to_integer :: Word256 -> Bool
to_word256_inverts_to_integer w256 =
  to_word256 (to_integer w256) == w256

to_integer_inverts_to_word256 :: Q.NonNegative Integer -> Bool
to_integer_inverts_to_word256 (Q.NonNegative n) =
  to_integer (to_word256 n) == n

add_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
add_matches (Q.NonNegative a) (Q.NonNegative b) =
  to_integer (to_word256 a `add` to_word256 b) == a + b

sub_matches :: Monotonic (Q.NonNegative Integer) -> Bool
sub_matches (Monotonic (Q.NonNegative a, Q.NonNegative b)) =
  to_integer (to_word256 a `sub` to_word256 b) == a - b

mul_lo_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
mul_lo_matches (Q.NonNegative a) (Q.NonNegative b) =
  let !mask128 = 0xffffffffffffffffffffffffffffffff
      !a_lo = a .&. mask128
      !b_lo = b .&. mask128

  in  to_word256 a_lo `mul` to_word256 b_lo == to_word256 (a_lo * b_lo)

mul_512_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
mul_512_matches (Q.NonNegative a) (Q.NonNegative b) =
  let !left = to_word256 a `mul_512` to_word256 b
      !rite = to_word512 (a * b)
  in  left == rite

-- main -----------------------------------------------------------------------

inverses :: TestTree
inverses = testGroup "inverses" [
    Q.testProperty "to_word256 . to_integer ~ id" $
      Q.withMaxSuccess 1000 to_word256_inverts_to_integer
  , Q.testProperty "to_integer . to_word256 ~ id (nonneg input)" $
      Q.withMaxSuccess 1000 to_integer_inverts_to_word256
  ]

arithmetic :: TestTree
arithmetic = testGroup "arithmetic" [
    Q.testProperty "addition matches (nonneg input)" $
      Q.withMaxSuccess 1000 add_matches
  , Q.testProperty "subtraction matches (nonneg, monotonic inputs)" $
      Q.withMaxSuccess 1000 sub_matches
  , Q.testProperty "multiplication matches (nonneg, low bits)" $
      Q.withMaxSuccess 1000 mul_512_matches
  ]

utils :: TestTree
utils = testGroup "utils" [
    Q.testProperty "mul_c matches integer multiplication" $
      Q.withMaxSuccess 1000 mul_c_matches
  , Q.testProperty "umul_hop: (hi * 2 ^ 64 + lo) = z + (x * y)" $
      Q.withMaxSuccess 1000 umul_hop_predicate_holds
  , Q.testProperty "umul_step: (hi * 2 ^ 64 + lo) = z + (x * y) + c" $
      Q.withMaxSuccess 1000 umul_step_predicate_holds
  , Q.testProperty "sub_mul matches integer sub_mul" $
      Q.withMaxSuccess 1000 sub_mul_matches
  ]

main :: IO ()
main = defaultMain $ testGroup "ppad-fw" [
    utils
  , inverses
  , arithmetic
  ]


