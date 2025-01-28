 {-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Data.Bits ((.|.), (.&.), (.>>.), (.^.))
import qualified Data.Bits as B
import qualified Data.Primitive.PrimArray as PA
import Data.Word.Extended
import GHC.Exts
import GHC.Word
import Prelude hiding (and, or, div, mod)
import qualified Prelude (div)
import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as Q

instance Q.Arbitrary Word256 where
  arbitrary = do
    w0 <- Q.arbitrary
    w1 <- Q.arbitrary
    w2 <- Q.arbitrary
    w3 <- Q.arbitrary
    pure (Word256 w0 w1 w2 w3)

newtype Monotonic = Monotonic (Integer, Integer)
  deriving Show

instance Q.Arbitrary Monotonic where
  arbitrary = do
    a <- Q.chooseInteger (0, 2 ^ (256 :: Int) - 1)
    b <- (Q.chooseInteger (0, 2 ^ (256 :: Int) - 1))
      `Q.suchThat` (\b -> b <= a)
    pure (Monotonic (a, b))

-- properties -----------------------------------------------------------------

-- addition / subtraction ----------------

add_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
add_matches (Q.NonNegative a) (Q.NonNegative b) =
  to_integer (to_word256 a `add` to_word256 b) == a + b

sub_matches :: Monotonic -> Bool
sub_matches (Monotonic (a, b)) =
  to_integer (to_word256 a `sub` to_word256 b) == a - b

-- multiplication ------------------------

mul_c_matches :: Word64 -> Word64 -> Bool
mul_c_matches a@(W64# a_ubox) b@(W64# b_ubox) =
  let c = fi a * fi b :: Integer
      c_hi = fi (c .>>. 64) :: Word64
      c_lo = fi (c .&. 0xffffffffffffffff) :: Word64

      !(# hi, lo #) = mul_c# a_ubox b_ubox
  in  (W64# hi) == c_hi && (W64# lo) == c_lo

-- (hi * 2 ^ 64 + lo) = z + (x * y)
umul_hop_predicate_holds :: Word64 -> Word64 -> Word64 -> Bool
umul_hop_predicate_holds z@(W64# z_ubox) x@(W64# x_ubox) y@(W64# y_ubox) =
  let !(# hi, lo #) = umul_hop# z_ubox x_ubox y_ubox
  in     fi (W64# hi) * 2 ^ (64 :: Int) + fi (W64# lo)
      == (fi z + (fi x * fi y) :: Integer)

-- (hi * 2 ^ 64 + lo) = z + (x * y) + c
umul_step_predicate_holds :: Word64 -> Word64 -> Word64 -> Word64 -> Bool
umul_step_predicate_holds
    z@(W64# z_ubox) x@(W64# x_ubox) y@(W64# y_ubox) c@(W64# c_ubox) =
  let !(# hi, lo #) = umul_step# z_ubox x_ubox y_ubox c_ubox
      !left = fi (W64# hi) * 2 ^ (64 :: Int) + fi (W64# lo) :: Integer
      !rite = fi z + (fi x * fi y) + fi c :: Integer
  in  left == rite

mul_lo_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
mul_lo_matches (Q.NonNegative a) (Q.NonNegative b) =
  let !mask128 = 0xffffffffffffffffffffffffffffffff
      !a_lo = a .&. mask128
      !b_lo = b .&. mask128

  in  to_word256 a_lo `mul` to_word256 b_lo == to_word256 (a_lo * b_lo)

-- division ------------------------------

quotrem_r_case0 :: H.Assertion
quotrem_r_case0 = do
  let !(P q r) = quotrem_r 2 4 4
  H.assertEqual mempty (P 9223372036854775809 0) (P q r)

quotrem_r_case1 :: H.Assertion
quotrem_r_case1 = do
  let !(P q r) = quotrem_r 0 4 2
  H.assertEqual mempty (P 2 0) (P q r)

quotrem_r_case2 :: H.Assertion
quotrem_r_case2 = do
  let !(P q r) = quotrem_r 4 0xffffffffffffffff (B.complement 4)
  H.assertEqual mempty (P 5 24) (P q r)

recip_2by1_case0 :: H.Assertion
recip_2by1_case0 = do
  let !q = recip_2by1 (B.complement 4)
  H.assertEqual mempty 5 q

recip_2by1_case1 :: H.Assertion
recip_2by1_case1 = do
  let !q = recip_2by1 (B.complement 0xff)
  H.assertEqual mempty 256 q

quotrem_2by1_case0 :: H.Assertion
quotrem_2by1_case0 = do
  let !d = B.complement 0xFF :: Word64
      !o = quotrem_2by1 8 4 d (recip_2by1 d)
  H.assertEqual mempty (P 8 2052) o

quotrem_by1_case0 :: H.Assertion
quotrem_by1_case0 = do
  qm <- PA.newPrimArray 2
  PA.setPrimArray qm 0 2 0
  let !u = PA.primArrayFromList [4, 8]
      !d = B.complement 0xFF :: Word64
  r <- quotrem_by1 qm u d
  q <- PA.unsafeFreezePrimArray qm
  H.assertEqual "quotient" (PA.primArrayFromList [8, 0]) q
  H.assertEqual "remainder" 2052 r

-- tests ----------------------------------------------------------------------



add_sub :: TestTree
add_sub = testGroup "addition & subtraction" [
    Q.testProperty "addition matches (nonneg)" $
      Q.withMaxSuccess 1000 add_matches
  , Q.testProperty "subtraction matches (nonneg, monotonic)" $
      Q.withMaxSuccess 1000 sub_matches
  ]

multiplication :: TestTree
multiplication = testGroup "arithmetic" [
   Q.testProperty "mul_c matches integer multiplication" $
     Q.withMaxSuccess 1000 mul_c_matches
  , Q.testProperty "umul_hop: (hi * 2 ^ 64 + lo) = z + (x * y)" $
      Q.withMaxSuccess 1000 umul_hop_predicate_holds
  , Q.testProperty "umul_step: (hi * 2 ^ 64 + lo) = z + (x * y) + c" $
      Q.withMaxSuccess 1000 umul_step_predicate_holds
  , Q.testProperty "mul matches (nonneg, low bits)" $
      Q.withMaxSuccess 1000 mul_lo_matches
  -- , Q.testProperty "division matches" $
  --     Q.withMaxSuccess 1000 div_matches
  -- , Q.testProperty "mod matches" $
  --     Q.withMaxSuccess 1000 mod_matches
  ]

main :: IO ()
main = defaultMain $ testGroup "ppad-fixed" [
    testGroup "property tests" [
      add_sub
    , multiplication
    ]
  , testGroup "unit tests" [
      H.testCase "quotrem_r matches case0" quotrem_r_case0
    , H.testCase "quotrem_r matches case1" quotrem_r_case1
    , H.testCase "quotrem_r matches case2" quotrem_r_case2
    , H.testCase "recip_2by1 matches case0" recip_2by1_case0
    , H.testCase "recip_2by1 matches case1" recip_2by1_case1
    , H.testCase "quotrem_2by1 matches case0" quotrem_2by1_case0
    , H.testCase "quotrem_by1 matches case0" quotrem_by1_case0
    ]
  ]

-- newtype Different a = Different (a, a)
--   deriving Show
--
-- instance (Q.Arbitrary a, Eq a) => Q.Arbitrary (Different a) where
--   arbitrary = do
--     a <- Q.arbitrary
--     b <- Q.arbitrary `Q.suchThat` (\b -> b /= a)
--     pure (Different (a, b))
--
-- -- second argument is no greater than first argument
-- -- second argument * third argument is no greater than first argument
-- newtype MulMonotonic = MulMonotonic (Integer, Integer, Integer)
--   deriving Show
--
-- instance Q.Arbitrary MulMonotonic where
--   arbitrary = do
--     Q.NonNegative a <- Q.arbitrary
--     m <- fmap fi (Q.arbitrary :: Q.Gen Word64)
--     Q.NonNegative b <-
--       Q.arbitrary `Q.suchThat` (\(Q.NonNegative b) -> b * m <= a)
--     pure (MulMonotonic (a, b, m))
--
-- newtype DivMonotonic = DivMonotonic (Integer, Integer)
--   deriving Show
--
-- instance Q.Arbitrary DivMonotonic where
--   arbitrary = do
--     a <- Q.chooseInteger (1, 2 ^ (256 :: Int) - 1)
--     b <- (Q.chooseInteger (1, 2 ^ (256 :: Int) - 1))
--       `Q.suchThat` (\b -> b <= a)
--     pure (DivMonotonic (a, b))
--
-- -- properties -----------------------------------------------------------------
--
-- lt_matches :: Different (Q.NonNegative Integer) -> Bool
-- lt_matches (Different (Q.NonNegative a, Q.NonNegative b))
--   | a < b     = to_word256 a `lt` to_word256 b
--   | otherwise = to_word256 b `lt` to_word256 a
--
-- gt_matches :: Different (Q.NonNegative Integer) -> Bool
-- gt_matches (Different (Q.NonNegative a, Q.NonNegative b))
--   | a > b     = to_word256 a `gt` to_word256 b
--   | otherwise = to_word256 b `gt` to_word256 a
--
-- to_word256_inverts_to_integer :: Word256 -> Bool
-- to_word256_inverts_to_integer w256 =
--   to_word256 (to_integer w256) == w256
--
-- to_integer_inverts_to_word256 :: Q.NonNegative Integer -> Bool
-- to_integer_inverts_to_word256 (Q.NonNegative n) =
--   to_integer (to_word256 n) == n
--
-- or_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
-- or_matches (Q.NonNegative a) (Q.NonNegative b) =
--   to_integer (to_word256 a `or` to_word256 b) == a .|. b
--
-- and_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
-- and_matches (Q.NonNegative a) (Q.NonNegative b) =
--   to_integer (to_word256 a `and` to_word256 b) == a .&. b
--
-- xor_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
-- xor_matches (Q.NonNegative a) (Q.NonNegative b) =
--   to_integer (to_word256 a `xor` to_word256 b) == a .^. b

-- sub_matches :: Monotonic -> Bool
-- sub_matches (Monotonic (a, b)) =
--   to_integer (to_word256 a `sub` to_word256 b) == a - b
--
-- div_matches :: DivMonotonic -> Bool
-- div_matches (DivMonotonic (a, b)) =
--   let !left = to_word256 a `div` to_word256 b
--       !rite = to_word256 (a `Prelude.div` b)
--   in  left == rite
--
-- mod_matches :: DivMonotonic -> Bool
-- mod_matches (DivMonotonic (a, b)) =
--   let !left = to_word256 a `mod` to_word256 b
--       !rite = to_word256 (a `rem` b)
--   in  left == rite
--
-- -- assertions ------------------------------------------------------------------
--
-- quotrem_r_case0 :: H.Assertion
-- quotrem_r_case0 = do
--   let !(P q r) = quotrem_r 2 4 4
--   H.assertEqual mempty (P 9223372036854775809 0) (P q r)
--
-- quotrem_r_case1 :: H.Assertion
-- quotrem_r_case1 = do
--   let !(P q r) = quotrem_r 0 4 2
--   H.assertEqual mempty (P 2 0) (P q r)
--
-- recip_2by1_case0 :: H.Assertion
-- recip_2by1_case0 = do
--   let !q = recip_2by1 (B.complement 4)
--   H.assertEqual mempty 5 q
--
-- recip_2by1_case1 :: H.Assertion
-- recip_2by1_case1 = do
--   let !q = recip_2by1 (B.complement 0xff)
--   H.assertEqual mempty 256 q

-- -- main -----------------------------------------------------------------------
--
-- comparison :: TestTree
-- comparison = testGroup "comparison" [
--     Q.testProperty "lt matches" $
--       Q.withMaxSuccess 1000 lt_matches
--   , Q.testProperty "gt matches" $
--       Q.withMaxSuccess 1000 gt_matches
--   ]
--
-- bits :: TestTree
-- bits = testGroup "bits" [
--     Q.testProperty "or matches" $
--       Q.withMaxSuccess 1000 or_matches
--   , Q.testProperty "and matches" $
--       Q.withMaxSuccess 1000 and_matches
--   , Q.testProperty "xor matches" $
--       Q.withMaxSuccess 1000 xor_matches
--   ]
--
-- inverses :: TestTree
-- inverses = testGroup "inverses" [
--     Q.testProperty "to_word256 . to_integer ~ id" $
--       Q.withMaxSuccess 1000 to_word256_inverts_to_integer
--   , Q.testProperty "to_integer . to_word256 ~ id (nonneg input)" $
--       Q.withMaxSuccess 1000 to_integer_inverts_to_word256
--   ]
--
-- arithmetic :: TestTree
-- arithmetic = testGroup "arithmetic" [
--     Q.testProperty "addition matches (nonneg)" $
--       Q.withMaxSuccess 1000 add_matches
--   , Q.testProperty "subtraction matches (nonneg, monotonic)" $
--       Q.withMaxSuccess 1000 sub_matches
--   , Q.testProperty "512-bit multiplication matches (nonneg, low bits)" $
--       Q.withMaxSuccess 1000 mul_512_matches
--   , Q.testProperty "division matches" $
--       Q.withMaxSuccess 1000 div_matches
--   , Q.testProperty "mod matches" $
--       Q.withMaxSuccess 1000 mod_matches
--   ]
--
-- utils :: TestTree
-- utils = testGroup "utils" [
--     Q.testProperty "mul_c matches integer multiplication" $
--       Q.withMaxSuccess 1000 mul_c_matches
--   , Q.testProperty "umul_hop: (hi * 2 ^ 64 + lo) = z + (x * y)" $
--       Q.withMaxSuccess 1000 umul_hop_predicate_holds
--   , Q.testProperty "umul_step: (hi * 2 ^ 64 + lo) = z + (x * y) + c" $
--       Q.withMaxSuccess 1000 umul_step_predicate_holds
--   ]
--
-- main :: IO ()
-- main = defaultMain $
--   testGroup "ppad-fixed" [
--     testGroup "property tests" [
--       comparison
--     , utils
--     , inverses
--     , bits
--     , arithmetic
--     ]
--   , testGroup "unit tests" [
--       H.testCase "quotrem_r matches case0" quotrem_r_case0
--     , H.testCase "quotrem_r matches case1" quotrem_r_case1
--     , H.testCase "recip_2by1 matches case0" recip_2by1_case0
--     , H.testCase "recip_2by1 matches case1" recip_2by1_case1
--     , H.testCase "quotrem_2by1 matches case0" quotrem_2by1_case0
--     ]
--   ]
--
