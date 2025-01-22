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

to_word256_inverts_to_integer :: Word256 -> Bool
to_word256_inverts_to_integer w256 =
  to_word256 (to_integer w256) == w256

to_integer_inverts_to_word256 :: Integer -> Bool
to_integer_inverts_to_word256 n =
  to_integer (to_word256 n) == n

conversion = testGroup "conversion" [
    Q.testProperty "to_word256 . to_integer ~ id" $
      Q.withMaxSuccess 1000 to_word256_inverts_to_integer
  , Q.testProperty "to_integer . to_word256 ~ id" $
      Q.withMaxSuccess 1000 to_integer_inverts_to_word256
  ]

main :: IO ()
main = defaultMain $ testGroup "ppad-fw" [
    conversion
  ]


