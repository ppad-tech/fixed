{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedTuples #-}

module Montgomery.Scalar (
    tests
  ) where

import qualified Data.Word.Wider as W
import qualified Numeric.Montgomery.Secp256k1.Scalar as S
import Test.Tasty
import qualified Test.Tasty.HUnit as H

-- modulus :: S.Montgomery
-- modulus = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

add :: H.Assertion
add = do
  H.assertBool mempty (W.eq_vartime (1 + 1) (S.from (1 + 1)))
  H.assertBool mempty (W.eq_vartime (0 + 1) (S.from (0 + 1)))
  let !m  = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
      !x  = 2 ^ (256 :: Word) - 1
      !mm = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
      !mx = 2 ^ (256 :: Word) - 1
  H.assertBool mempty (W.eq_vartime 0 (S.from mm))
  H.assertBool mempty (W.eq_vartime (x - m) (S.from (mx - mm)))

tests :: TestTree
tests = testGroup "montgomery tests (scalar)" [
    H.testCase "add" add
  ]

