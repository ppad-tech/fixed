module Wide (
    tests
  ) where

import qualified Data.Word.Wide as W
import Prelude hiding (div, recip)
import Test.Tasty
import qualified Test.Tasty.HUnit as H

tests :: TestTree
tests = testGroup "wide unit tests" [
    H.testCase "mul_c, case 0" mul_c_case0
  , H.testCase "div1by1, case 0" div1by1_case0
  , H.testCase "div1by1, case 1" div1by1_case1
  , H.testCase "recip, case 0" recip_case0
  , H.testCase "recip, case 1" recip_case1
  , H.testCase "recip, case 2" recip_case2
  , H.testCase "div2by1, case 0" div2by1_case0
  , H.testCase "div2by1, case 1" div2by1_case1
  , H.testCase "div2by1, case 2" div2by1_case2
  , H.testCase "div2by1, case 3 (GHC.Exts reference)" div2by1_case3
  , H.testCase "div2by1, case 4 (GHC.Exts reference)" div2by1_case4
  , H.testCase "div2by1, case 5 (GHC.Exts reference)" div2by1_case5
  ]

mul_c_case0 :: H.Assertion
mul_c_case0 = do
  let a = 4294967294
      b = 2
  H.assertEqual "matches" (W.wide 8589934588 0) (W.mul_c a b)

div1by1_case0 :: H.Assertion
div1by1_case0 = do
  let dnd = 4294967294
      dnb = 32
      div = 4294967293
      dib = 32
      v0  = W.div1by1 dnd dnb div dib
  H.assertEqual "matches" 1 v0

div1by1_case1 :: H.Assertion
div1by1_case1 = do
  let dnd = 4294967294
      dnb = 32
      div = 2
      dib = 2
      v0  = W.div1by1 dnd dnb div dib
  H.assertEqual "matches" 2147483647 v0

recip_case0 :: H.Assertion
recip_case0 = do
  let d = 18446744073709551606
      e = W.recip d
  H.assertEqual "matches" 10 e

recip_case1 :: H.Assertion
recip_case1 = do
  let d = 0x8000000000000000  -- 2^63
      e = W.recip d
  H.assertEqual "matches" 0xffffffffffffffff e

recip_case2 :: H.Assertion
recip_case2 = do
  let d = 0x8000000000000001  -- 2^63 + 1
      e = W.recip d
  H.assertEqual "matches" 0xfffffffffffffffc e

div2by1_case0 :: H.Assertion
div2by1_case0 = do
  let d  = maxBound - 1
      r  = W.div2by1 (W.wide (maxBound - 63) (maxBound - 2)) d
      e  = (maxBound, maxBound - 65)
  H.assertEqual "matches" e r

div2by1_case1 :: H.Assertion
div2by1_case1 = do
  let d  = 0x8000000000000000  -- 2^63
      r  = W.div2by1 (W.wide 0xffffffffffffffff 0x7fffffffffffffff) d
      e  = (0xffffffffffffffff, 0x7fffffffffffffff)
  H.assertEqual "matches" e r

div2by1_case2 :: H.Assertion
div2by1_case2 = do
  let d  = 0x8000000000000001  -- 2^63 + 1
      r  = W.div2by1 (W.wide 0x0000000000000000 0x8000000000000000) d
      e  = (0xfffffffffffffffe, 0x2)
  H.assertEqual "matches" e r

div2by1_case3 :: H.Assertion
div2by1_case3 = do
  let d  = maxBound - 1
      r  = W.div2by1 (W.wide (maxBound - 63) (maxBound - 2)) d
      e  = W.quotrem2by1 (W.wide (maxBound - 63) (maxBound - 2)) d
  H.assertEqual "matches" e r

div2by1_case4 :: H.Assertion
div2by1_case4 = do
  let d  = 0x8000000000000000  -- 2^63
      r  = W.div2by1 (W.wide 0xffffffffffffffff 0x7fffffffffffffff) d
      e  = W.quotrem2by1 (W.wide 0xffffffffffffffff 0x7fffffffffffffff) d
  H.assertEqual "matches" e r

div2by1_case5 :: H.Assertion
div2by1_case5 = do
  let d  = 0x8000000000000001  -- 2^63 + 1
      r  = W.div2by1 (W.wide 0x0000000000000000 0x8000000000000000) d
      e  = W.quotrem2by1 (W.wide 0x0000000000000000 0x8000000000000000) d
  H.assertEqual "matches" e r

