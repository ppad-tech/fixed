{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

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

to_word256_inverts_to_integer :: Word256 -> Bool
to_word256_inverts_to_integer w256 =
  to_word256 (to_integer w256) == w256

-- doesn't hold for negative inputs
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


main :: IO ()
main = defaultMain $ testGroup "ppad-fw" [
    inverses
  , arithmetic
  ]


