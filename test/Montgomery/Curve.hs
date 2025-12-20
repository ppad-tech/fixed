{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Montgomery.Curve (
    tests
  ) where

import qualified Data.Word.Wider as W
import qualified GHC.Num.Integer as I
import GHC.Natural
import qualified Numeric.Montgomery.Secp256k1.Curve as C
import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as Q

-- generic modular exponentiation
-- b ^ e mod m
modexp :: Integer -> Natural -> Natural -> Integer
modexp b (fromIntegral -> e) p = case I.integerPowMod# b e p of
  (# fromIntegral -> n | #) -> n
  (# | _ #) -> error "bang"
{-# INLINE modexp #-}

-- modulus
m :: W.Wider
m = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

-- modulus
mm :: C.Montgomery
mm = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

repr :: H.Assertion
repr = H.assertBool mempty (W.eq_vartime 0 (C.from mm))

add_case :: String -> W.Wider -> W.Wider -> W.Wider -> H.Assertion
add_case t a b s = do
  H.assertEqual "sanity" ((W.from a + W.from b) `mod` W.from m) (W.from s)
  H.assertBool t (W.eq_vartime s (C.from (C.to a + C.to b)))

add :: H.Assertion
add = do
  add_case "small" 1 2 3
  add_case "wrap to 0 mod m"
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2E 1 0
  add_case "wrap to 1"
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2D 3 1
  add_case "random"
    0x000123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCD
    0x0FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA987654321
    0x0FEEEEEEEEEEEEEEFEEEEEEEEEEEEEEEFEEEEEEEEEEEEEEEFEEEEEEEEEEEEEEE
  add_case "near R"
    0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    0x5555555555555555555555555555555555555555555555555555555555555555
    0x00000000000000000000000000000000000000000000000000000001000003D0

sub_case :: String -> W.Wider -> W.Wider -> W.Wider -> H.Assertion
sub_case t b a d = do
  H.assertEqual "sanity" ((W.from b - W.from a) `mod` W.from m) (W.from d)
  H.assertBool t (W.eq_vartime d (C.from (C.to b - C.to a)))

sub :: H.Assertion
sub = do
  sub_case "small" 3 2 1
  sub_case "wrap from 0 mod m" 0 1
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2E
  sub_case "wrap to 0" 1 1 0
  sub_case "random"
    0x0FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA987654321
    0x000123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCD
    0x0FECA8641FDB975320ECA8641FDB975320ECA8641FDB975320ECA8641FDB9754
  sub_case "near R"
    0x00000000000000000000000000000000000000000000000000000001000003D0
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2E
    0x00000000000000000000000000000000000000000000000000000001000003D1

mul_case :: String -> W.Wider -> W.Wider -> W.Wider -> H.Assertion
mul_case t a b p = do
  H.assertEqual "sanity" ((W.from a * W.from b) `mod` W.from m) (W.from p)
  H.assertBool t (W.eq_vartime p (C.from (C.to a * C.to b)))

mul :: H.Assertion
mul = do
  mul_case "small" 2 3 6
  mul_case "wrap to 1 mod m"
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2E
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2E
    0x1
  mul_case "zero"
    0x000123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCD
    0x0
    0x0
  mul_case "random"
    0x000123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCD
    0x0FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA987654321
    0xCEF9C520FC3502A4BA6F1CE3B2550511D5E474A66875077EF159DE87E15148FC
  mul_case "near R"
    0x00000000000000000000000000000000000000000000000000000001000003D1
    0x00000000000000000000000000000000000000000000000000000001000003D1
    0x000000000000000000000000000000000000000000000001000007A2000E90A1

instance Q.Arbitrary W.Wider where
  arbitrary = fmap W.to Q.arbitrary

instance Q.Arbitrary C.Montgomery where
  arbitrary = fmap C.to Q.arbitrary

add_matches :: W.Wider -> W.Wider -> Bool
add_matches a b =
  let ma = C.to a
      mb = C.to b
      ia = W.from a
      ib = W.from b
      im = W.from m
  in  W.eq_vartime (W.to ((ia + ib) `mod` im)) (C.from (ma + mb))

mul_matches :: W.Wider -> W.Wider -> Bool
mul_matches a b =
  let ma = C.to a
      mb = C.to b
      ia = W.from a
      ib = W.from b
      im = W.from m
  in  W.eq_vartime (W.to ((ia * ib) `mod` im)) (C.from (ma * mb))

sqr_matches :: W.Wider -> Bool
sqr_matches a =
  let ma = C.to a
      ia = W.from a
      im = W.from m
  in  W.eq_vartime (W.to ((ia * ia) `mod` im)) (C.from (C.sqr ma))

exp_matches :: C.Montgomery -> W.Wider -> Bool
exp_matches a b =
  let ia = W.from (C.from a)
      nb = fromIntegral (W.from b)
      nm = fromIntegral (W.from m)
  in  W.eq_vartime (W.to (modexp ia nb nm)) (C.from (C.exp a b))

inv_valid :: Q.NonZero C.Montgomery -> Bool
inv_valid (Q.NonZero s) = C.eq_vartime (C.inv s * s) 1

odd_correct :: C.Montgomery -> Bool
odd_correct w = C.odd w == I.integerTestBit (W.from (C.from w)) 0

tests :: TestTree
tests = testGroup "montgomery tests (curve)" [
    H.testCase "representation" repr
  , H.testCase "add" add
  , H.testCase "sub" sub
  , H.testCase "mul" mul
  , Q.testProperty "a + b mod m ~ ma + mb" $ Q.withMaxSuccess 500 add_matches
  , Q.testProperty "a * b mod m ~ ma * mb" $ Q.withMaxSuccess 500 mul_matches
  , Q.testProperty "a ^ 2 mod m ~ ma ^ 2"  $ Q.withMaxSuccess 500 sqr_matches
  , Q.testProperty "a ^ b mod m ~ ma ^ mb" $ Q.withMaxSuccess 500 exp_matches
  , Q.testProperty "n ^ -1 mod m * n ~ 1"  $ Q.withMaxSuccess 500 inv_valid
  , Q.testProperty "odd m ~ odd (from m)"  $ Q.withMaxSuccess 500 odd_correct
  ]

