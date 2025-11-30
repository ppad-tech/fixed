{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedTuples #-}

module Montgomery.Curve (
    tests
  ) where

import qualified Data.Word.Wider as W
import qualified Numeric.Montgomery.Secp256k1.Curve as C
import Test.Tasty
import qualified Test.Tasty.HUnit as H

add :: H.Assertion
add = do
  H.assertBool mempty (W.eq_vartime (1 + 1) (C.from (1 + 1)))
  H.assertBool mempty (W.eq_vartime (0 + 1) (C.from (0 + 1)))
  let !m  = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
      !x  = 2 ^ (256 :: Word) - 1
      !mm = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
      !mx = 2 ^ (256 :: Word) - 1
  H.assertBool mempty (W.eq_vartime 0 (C.from mm))
  H.assertBool mempty (W.eq_vartime (x - m) (C.from (mx - mm)))

tests :: TestTree
tests = testGroup "montgomery tests (curve)" [
    H.testCase "add" add
  ]

