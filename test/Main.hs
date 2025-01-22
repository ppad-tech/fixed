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

-- properties -----------------------------------------------------------------

mul_c_matches :: Word64 -> Word64 -> Bool
mul_c_matches a b =
  let c = fi a * fi b :: Integer
      c_hi = fi (c .>>. 64) :: Word64
      c_lo = fi (c .&. 0xffffffffffffffff) :: Word64

      W64P hi lo = mul_c a b
  in  hi == c_hi && lo == c_lo

-- (hi * 2 ^ 64 + lo) = z + (x * y)
umul_hop_predicate_holds :: Word64 -> Word64 -> Word64 -> Bool
umul_hop_predicate_holds z x y =
  let !(W64P hi lo) = umul_hop z x y
  in  fi hi * 2 ^ (64 :: Int) + fi lo == (fi z + (fi x * fi y) :: Integer)

-- (hi * 2 ^ 64 + lo) = z + (x * y) + c
umul_step_predicate_holds :: Word64 -> Word64 -> Word64 -> Word64 -> Bool
umul_step_predicate_holds z x y c =
  let !(W64P hi lo) = umul_step z x y c
      !left = fi hi * 2 ^ (64 :: Int) + fi lo :: Integer
      !rite = fi z + (fi x * fi y) + fi c :: Integer
  in  left == rite

to_word256_inverts_to_integer :: Word256 -> Bool
to_word256_inverts_to_integer w256 =
  to_word256 (to_integer w256) == w256

to_integer_inverts_to_word256 :: Q.NonNegative Integer -> Bool
to_integer_inverts_to_word256 (Q.NonNegative n) =
  to_integer (to_word256 n) == n

addition_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
addition_matches (Q.NonNegative a) (Q.NonNegative b) =
  to_integer (to_word256 a `add` to_word256 b) == a + b

subtraction_matches :: Monotonic (Q.NonNegative Integer) -> Bool
subtraction_matches (Monotonic (Q.NonNegative a, Q.NonNegative b)) =
  to_integer (to_word256 a `sub` to_word256 b) == a - b

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
      Q.withMaxSuccess 1000 addition_matches
  , Q.testProperty "subtraction matches (nonneg, monotonic inputs)" $
      Q.withMaxSuccess 1000 subtraction_matches
  ]

utils :: TestTree
utils = testGroup "utils" [
    Q.testProperty "mul_c matches integer multiplication" $
      Q.withMaxSuccess 1000 mul_c_matches
  , Q.testProperty "umul_hop: (hi * 2 ^ 64 + lo) = z + (x * y)" $
      Q.withMaxSuccess 1000 umul_hop_predicate_holds
  , Q.testProperty "umul_step: (hi * 2 ^ 64 + lo) = z + (x * y) + c" $
      Q.withMaxSuccess 1000 umul_step_predicate_holds
  ]

main :: IO ()
main = defaultMain $ testGroup "ppad-fw" [
    utils
  , inverses
  , arithmetic
  ]


