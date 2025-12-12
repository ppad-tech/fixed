{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Montgomery.Scalar (
    tests
  ) where

import qualified Data.Word.Wider as W
import qualified GHC.Num.Integer as I
import GHC.Natural
import qualified Numeric.Montgomery.Secp256k1.Scalar as S
import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as Q

-- generic modular exponentiation
-- b ^ e mod m
modexp :: Integer -> Natural -> Natural -> Integer
modexp b (fromIntegral -> e) q = case I.integerPowMod# b e q of
  (# fromIntegral -> n | #) -> n
  (# | _ #) -> error "bang"
{-# INLINE modexp #-}

-- modulus
m :: W.Wider
m = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

-- modulus
mm :: S.Montgomery
mm = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

repr :: H.Assertion
repr = H.assertBool mempty (W.eq_vartime 0 (S.from mm))

add_case :: String -> W.Wider -> W.Wider -> W.Wider -> H.Assertion
add_case t a b s = do
  H.assertEqual "sanity" ((W.from a + W.from b) `mod` W.from m) (W.from s)
  H.assertBool t (W.eq_vartime s (S.from (S.to a + S.to b)))

add :: H.Assertion
add = do
  add_case "small" 1 2 3
  add_case "wrap to 0 mod m"
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140 1 0
  add_case "wrap to 1"
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD036413F 3 1
  add_case "random"
    0x000123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCD
    0x0FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA987654321
    0x0FEEEEEEEEEEEEEEFEEEEEEEEEEEEEEEFEEEEEEEEEEEEEEEFEEEEEEEEEEEEEEE
  add_case "near R"
    0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    0x5555555555555555555555555555555555555555555555555555555555555555
    0x000000000000000000000000000000014551231950B75FC4402DA1732FC9BEBE

sub_case :: String -> W.Wider -> W.Wider -> W.Wider -> H.Assertion
sub_case t b a d = do
  H.assertEqual "sanity" ((W.from b - W.from a) `mod` W.from m) (W.from d)
  H.assertBool t (W.eq_vartime d (S.from (S.to b - S.to a)))

sub :: H.Assertion
sub = do
  sub_case "small" 3 2 1
  sub_case "wrap from 0 mod m" 0 1
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
  sub_case "wrap to 0" 1 1 0
  sub_case "random"
    0x0FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA987654321
    0x000123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCD
    0x0FECA8641FDB975320ECA8641FDB975320ECA8641FDB975320ECA8641FDB9754
  sub_case "near R"
    0x000000000000000000000000000000014551231950B75FC4402DA1732FC9BEBE
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
    0x000000000000000000000000000000014551231950B75FC4402DA1732FC9BEBF

mul_case :: String -> W.Wider -> W.Wider -> W.Wider -> H.Assertion
mul_case t a b p = do
  H.assertEqual "sanity" ((W.from a * W.from b) `mod` W.from m) (W.from p)
  H.assertBool t (W.eq_vartime p (S.from (S.to a * S.to b)))

mul :: H.Assertion
mul = do
  mul_case "small" 2 3 6
  mul_case "wrap to 1 mod m"
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
    0x1
  mul_case "zero"
    0x000123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCD
    0x0
    0x0
  mul_case "random"
    0x000123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCD
    0x0FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA987654321
    0x1A9B526FE2B5CE72CE59A8E81612BC5785CED8C6B231B643B36DA80BE2A60636
  mul_case "near R"
    0x000000000000000000000000000000014551231950B75FC4402DA1732FC9BEBF
    0x000000000000000000000000000000014551231950B75FC4402DA1732FC9BEBF
    0x9D671CD581C69BC5E697F5E45BCD07C6741496C20E7CF878896CF21467D7D140

instance Q.Arbitrary W.Wider where
  arbitrary = fmap W.to Q.arbitrary

instance Q.Arbitrary S.Montgomery where
  arbitrary = fmap S.to Q.arbitrary

add_matches :: W.Wider -> W.Wider -> Bool
add_matches a b =
  let ma = S.to a
      mb = S.to b
      ia = W.from a
      ib = W.from b
      im = W.from m
  in  W.eq_vartime (W.to ((ia + ib) `mod` im)) (S.from (ma + mb))

mul_matches :: W.Wider -> W.Wider -> Bool
mul_matches a b =
  let ma = S.to a
      mb = S.to b
      ia = W.from a
      ib = W.from b
      im = W.from m
  in  W.eq_vartime (W.to ((ia * ib) `mod` im)) (S.from (ma * mb))

exp_matches :: S.Montgomery -> W.Wider -> Bool
exp_matches a b =
  let ia = W.from (S.from a)
      nb = fromIntegral (W.from b)
      nm = fromIntegral (W.from m)
  in  W.eq_vartime (W.to (modexp ia nb nm)) (S.from (S.exp a b))

inv_valid :: Q.NonZero S.Montgomery -> Bool
inv_valid (Q.NonZero s) = S.eq_vartime (S.inv s * s) 1

tests :: TestTree
tests = testGroup "montgomery tests (scalar)" [
    H.testCase "representation" repr
  , H.testCase "add" add
  , H.testCase "sub" sub
  , H.testCase "mul" mul
  , Q.testProperty "a + b mod m ~ ma + mb" $ Q.withMaxSuccess 500 add_matches
  , Q.testProperty "a * b mod m ~ ma * mb" $ Q.withMaxSuccess 500 mul_matches
  , Q.testProperty "a ^ b mod m ~ ma ^ mb" $ Q.withMaxSuccess 500 exp_matches
  , Q.testProperty "n ^ -1 mod m * n ~ 1"  $ Q.withMaxSuccess 500 inv_valid
  ]

