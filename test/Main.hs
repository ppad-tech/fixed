{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.ST
import Data.Bits ((.|.), (.&.), (.>>.), (.^.))
import qualified Data.Bits as B
import qualified Data.Primitive.PrimArray as PA
import Data.Word (Word64)
import Data.Word.Extended
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

newtype Different a = Different (a, a)
  deriving Show

instance (Q.Arbitrary a, Eq a) => Q.Arbitrary (Different a) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary `Q.suchThat` (\b -> b /= a)
    pure (Different (a, b))

-- second argument is no greater than first argument
newtype Monotonic = Monotonic (Integer, Integer)
  deriving Show

instance Q.Arbitrary Monotonic where
  arbitrary = do
    a <- Q.chooseInteger (0, 2 ^ (256 :: Int) - 1)
    b <- (Q.chooseInteger (0, 2 ^ (256 :: Int) - 1))
      `Q.suchThat` (\b -> b <= a)
    pure (Monotonic (a, b))

-- second argument * third argument is no greater than first argument
newtype MulMonotonic = MulMonotonic (Integer, Integer, Integer)
  deriving Show

instance Q.Arbitrary MulMonotonic where
  arbitrary = do
    Q.NonNegative a <- Q.arbitrary
    m <- fmap fi (Q.arbitrary :: Q.Gen Word64)
    Q.NonNegative b <-
      Q.arbitrary `Q.suchThat` (\(Q.NonNegative b) -> b * m <= a)
    pure (MulMonotonic (a, b, m))

newtype DivMonotonic = DivMonotonic (Integer, Integer)
  deriving Show

instance Q.Arbitrary DivMonotonic where
  arbitrary = do
    a <- Q.chooseInteger (1, 2 ^ (256 :: Int) - 1)
    b <- (Q.chooseInteger (1, 2 ^ (256 :: Int) - 1))
      `Q.suchThat` (\b -> b <= a)
    pure (DivMonotonic (a, b))

-- properties -----------------------------------------------------------------

lt_matches :: Different (Q.NonNegative Integer) -> Bool
lt_matches (Different (Q.NonNegative a, Q.NonNegative b))
  | a < b     = to_word256 a `lt` to_word256 b
  | otherwise = to_word256 b `lt` to_word256 a

gt_matches :: Different (Q.NonNegative Integer) -> Bool
gt_matches (Different (Q.NonNegative a, Q.NonNegative b))
  | a > b     = to_word256 a `gt` to_word256 b
  | otherwise = to_word256 b `gt` to_word256 a

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

to_word256_inverts_to_integer :: Word256 -> Bool
to_word256_inverts_to_integer w256 =
  to_word256 (to_integer w256) == w256

to_integer_inverts_to_word256 :: Q.NonNegative Integer -> Bool
to_integer_inverts_to_word256 (Q.NonNegative n) =
  to_integer (to_word256 n) == n

or_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
or_matches (Q.NonNegative a) (Q.NonNegative b) =
  to_integer (to_word256 a `or` to_word256 b) == a .|. b

and_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
and_matches (Q.NonNegative a) (Q.NonNegative b) =
  to_integer (to_word256 a `and` to_word256 b) == a .&. b

xor_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
xor_matches (Q.NonNegative a) (Q.NonNegative b) =
  to_integer (to_word256 a `xor` to_word256 b) == a .^. b

add_matches :: Q.NonNegative Integer -> Q.NonNegative Integer -> Bool
add_matches (Q.NonNegative a) (Q.NonNegative b) =
  to_integer (to_word256 a `add` to_word256 b) == a + b

sub_matches :: Monotonic -> Bool
sub_matches (Monotonic (a, b)) =
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

div_matches :: DivMonotonic -> Bool
div_matches (DivMonotonic (a, b)) =
  let !left = to_word256 a `div` to_word256 b
      !rite = to_word256 (a `Prelude.div` b)
  in  left == rite

div_pure_matches :: DivMonotonic -> Bool
div_pure_matches (DivMonotonic (a, b)) =
  let !left = to_word256 a `div_pure` to_word256 b
      !rite = to_word256 (a `Prelude.div` b)
  in  left == rite

mod_matches :: DivMonotonic -> Bool
mod_matches (DivMonotonic (a, b)) =
  let !left = to_word256 a `mod` to_word256 b
      !rite = to_word256 (a `rem` b)
  in  left == rite

mod_pure_matches :: DivMonotonic -> Bool
mod_pure_matches (DivMonotonic (a, b)) =
  let !left = to_word256 a `mod_pure` to_word256 b
      !rite = to_word256 (a `rem` b)
  in  left == rite

-- assertions ------------------------------------------------------------------

quotrem_r_case0 :: H.Assertion
quotrem_r_case0 = do
  let !(P q r) = quotrem_r 2 4 4
  H.assertEqual mempty (P 9223372036854775809 0) (P q r)

quotrem_r_case1 :: H.Assertion
quotrem_r_case1 = do
  let !(P q r) = quotrem_r 0 4 2
  H.assertEqual mempty (P 2 0) (P q r)

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
  let (q, r) = runST $ do
        quo <- PA.newPrimArray 4
        PA.setPrimArray quo 0 4 0
        let !u = PA.primArrayFromList [8, 4]
            !d = B.complement 0xFF :: Word64
        re <- quotrem_by1 quo u d
        qu <- PA.unsafeFreezePrimArray quo
        pure (qu, re)
  let pec_array = PA.primArrayFromList [4, 0, 0, 0]
      pec_rem   = 1032
  H.assertEqual "remainder matches" pec_rem r
  H.assertEqual "quotient matches" pec_array q

quotrem_by1_gen_case0 :: H.Assertion
quotrem_by1_gen_case0 = do
  let !u = Word576 8 4 0 0 0 0 0 0 0
      !d = B.complement 0xFF :: Word64
      !(Word640 q r) = quotrem_by1_gen u 2 d
  let pec_quo = Word576 4 0 0 0 0 0 0 0 0
      pec_rem = 1032
  H.assertEqual "remainder matches" pec_rem r
  H.assertEqual "quotient matches" pec_quo q

quotrem_by1_case1 :: H.Assertion
quotrem_by1_case1 = do
  let (q, r) = runST $ do
        quo <- PA.newPrimArray 4
        PA.setPrimArray quo 0 4 0
        let !u = PA.primArrayFromList [8, 26]
            !d = B.complement 0xFF :: Word64
        re <- quotrem_by1 quo u d
        qu <- PA.unsafeFreezePrimArray quo
        pure (qu, re)
  let pec_array = PA.primArrayFromList [26, 0, 0, 0]
      pec_rem   = 6664
  H.assertEqual "remainder matches" pec_rem r
  H.assertEqual "quotient matches" pec_array q

quotrem_by1_gen_case1 :: H.Assertion
quotrem_by1_gen_case1 = do
  let !u = Word576 8 26 0 0 0 0 0 0 0
      !d = B.complement 0xFF :: Word64
      !(Word640 q r) = quotrem_by1_gen u 2 d
  let pec_quo = Word576 26 0 0 0 0 0 0 0 0
      pec_rem = 6664
  H.assertEqual "remainder matches" pec_rem r
  H.assertEqual "quotient matches" pec_quo q

quotrem_knuth_gen_case0 :: H.Assertion
quotrem_knuth_gen_case0 = do
  let !u = Word576
        2162362899639802732
        8848548347662387477
        13702897166684377657
        16799544643779908154
        1
        0 0 0 0
      !d = Word256
        16950798510782491100
        2612788699139816405
        5146719872810836952
        14966148379609982000
      !(Word1152 q nu) = quotrem_knuth_gen u 5 d 4
      !pec_q = Word576 2 0 0 0 0 0 0 0 0
      !pec_u = Word576
        5154254025493923764
        3622970949382754665
        3409457421062703753
        5313991958269495770
        0
        0 0 0 0
  H.assertEqual "divisor matches" pec_u nu
  H.assertEqual "quotient matches" pec_q q

quotrem_knuth_case0 :: H.Assertion
quotrem_knuth_case0 = do
  let (q, u) = runST $ do
        quo <- PA.newPrimArray 5
        PA.setPrimArray quo 0 5 0
        u_arr <- PA.newPrimArray 5
        PA.writePrimArray u_arr 0 2162362899639802732
        PA.writePrimArray u_arr 1 8848548347662387477
        PA.writePrimArray u_arr 2 13702897166684377657
        PA.writePrimArray u_arr 3 16799544643779908154
        PA.writePrimArray u_arr 4 1
        let !d = PA.primArrayFromList [
                16950798510782491100
              , 2612788699139816405
              , 5146719872810836952
              , 14966148379609982000
              ]
        quotrem_knuth quo u_arr d
        qf <- PA.unsafeFreezePrimArray quo
        uf <- PA.unsafeFreezePrimArray u_arr
        pure (qf, uf)
  let pec_q = PA.primArrayFromList [2, 0, 0, 0, 0]
      pec_u = PA.primArrayFromList [
          5154254025493923764
        , 3622970949382754665
        , 3409457421062703753
        , 5313991958269495770
        , 0
        ]
  H.assertEqual "divisor matches" pec_u u
  H.assertEqual "quotient matches" pec_q q

quotrem_case0 :: H.Assertion
quotrem_case0 = do
  let (q, r) = runST $ do
        quo <- PA.newPrimArray 5
        PA.setPrimArray quo 0 5 (0 :: Word64)
        let !u = PA.primArrayFromList
              [0x1234567890ABCDEF, 0xFEDCBA0987654321, 0x123456789ABCDEF0]
            !d = PA.primArrayFromList
              [0x0, 0x0, 0x1, 0x100000000]
        rf <- quotrem quo u d
        qf <- PA.unsafeFreezePrimArray quo
        pure (qf, rf)
  let pec_q = PA.primArrayFromList [0, 0, 0, 0, 0]
      pec_r = Word256
        1311768467294899695
        18364757930599072545
        1311768467463790320
        0
  H.assertEqual "remainder matches" pec_r r
  H.assertEqual "quotient matches" pec_q q

quotrem_case1 :: H.Assertion
quotrem_case1 = do
  let (q, r) = runST $ do
        quo <- PA.newPrimArray 5
        PA.setPrimArray quo 0 5 0
        let !u = PA.primArrayFromList [
                5152276743337338587
              , 6823823105342984773
              , 12649096328525870222
              , 8811572179372364942
              ]
            !d = PA.primArrayFromList [
                8849385646123010679
              , 653197174784954101
              , 1286679968202709238
              , 3741537094902495500
              ]

        rf <- quotrem quo u d
        qf <- PA.unsafeFreezePrimArray quo
        pure (qf, rf)
  let pec_q = PA.primArrayFromList [2, 0, 0, 0, 0]
      pec_r = Word256
        5900249524800868845
        5517428755773076570
        10075736392120451746
        1328497989567373942

  H.assertEqual "remainder matches" pec_r r
  H.assertEqual "quotient matches" pec_q q

quotrem_gen_case0 :: H.Assertion
quotrem_gen_case0 = do
  let !u = Word576
        0x1234567890ABCDEF
        0xFEDCBA0987654321
        0x123456789ABCDEF0
        0 0 0 0 0 0
      !d = Word256 0x0 0x0 0x1 0x100000000
      !(Word832 q r) = quotrem_gen u d
      !pec_q = Word576 0 0 0 0 0 0 0 0 0
      !pec_r = Word256
        1311768467294899695
        18364757930599072545
        1311768467463790320
        0
  H.assertEqual "remainder matches" pec_r r
  H.assertEqual "quotient matches" pec_q q

quotrem_gen_case1 :: H.Assertion
quotrem_gen_case1 = do
  let !u = Word576
        5152276743337338587
        6823823105342984773
        12649096328525870222
        8811572179372364942
        0 0 0 0 0
      !d = Word256
        8849385646123010679
        653197174784954101
        1286679968202709238
        3741537094902495500
      !(Word832 q r) = quotrem_gen u d
      !pec_q = Word576 2 0 0 0 0 0 0 0 0
      !pec_r = Word256
        5900249524800868845
        5517428755773076570
        10075736392120451746
        1328497989567373942
  H.assertEqual "remainder matches" pec_r r
  H.assertEqual "quotient matches" pec_q q

-- main -----------------------------------------------------------------------

comparison :: TestTree
comparison = testGroup "comparison" [
    Q.testProperty "lt matches" $
      Q.withMaxSuccess 1000 lt_matches
  , Q.testProperty "gt matches" $
      Q.withMaxSuccess 1000 gt_matches
  ]

bits :: TestTree
bits = testGroup "bits" [
    Q.testProperty "or matches" $
      Q.withMaxSuccess 1000 or_matches
  , Q.testProperty "and matches" $
      Q.withMaxSuccess 1000 and_matches
  , Q.testProperty "xor matches" $
      Q.withMaxSuccess 1000 xor_matches
  ]

inverses :: TestTree
inverses = testGroup "inverses" [
    Q.testProperty "to_word256 . to_integer ~ id" $
      Q.withMaxSuccess 1000 to_word256_inverts_to_integer
  , Q.testProperty "to_integer . to_word256 ~ id (nonneg input)" $
      Q.withMaxSuccess 1000 to_integer_inverts_to_word256
  ]

arithmetic :: TestTree
arithmetic = testGroup "arithmetic" [
    Q.testProperty "addition matches (nonneg)" $
      Q.withMaxSuccess 1000 add_matches
  , Q.testProperty "subtraction matches (nonneg, monotonic)" $
      Q.withMaxSuccess 1000 sub_matches
  , Q.testProperty "multiplication matches (nonneg, low bits)" $
      Q.withMaxSuccess 1000 mul_512_matches
  , Q.testProperty "division matches" $
      Q.withMaxSuccess 1000 div_matches
  , Q.testProperty "pure division matches" $
      Q.withMaxSuccess 1000 div_pure_matches
  , Q.testProperty "mod matches" $
      Q.withMaxSuccess 1000 mod_matches
  , Q.testProperty "pure mod matches" $
      Q.withMaxSuccess 1000 mod_pure_matches
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
main = defaultMain $
  testGroup "ppad-fixed" [
    testGroup "property tests" [
      comparison
    , utils
    , inverses
    , bits
    , arithmetic
    ]
  , testGroup "unit tests" [
      H.testCase "quotrem_r matches case0" quotrem_r_case0
    , H.testCase "quotrem_r matches case1" quotrem_r_case1
    , H.testCase "recip_2by1 matches case0" recip_2by1_case0
    , H.testCase "recip_2by1 matches case1" recip_2by1_case1
    , H.testCase "quotrem_2by1 matches case0" quotrem_2by1_case0
    , H.testCase "quotrem_by1 matches case0" quotrem_by1_case0
    , H.testCase "quotrem_by1 matches case1" quotrem_by1_case1
    , H.testCase "quotrem_by1_gen matches case0" quotrem_by1_gen_case0
    , H.testCase "quotrem_by1_gen matches case1" quotrem_by1_gen_case1
    , H.testCase "quotrem_knuth_gen matches case0" quotrem_knuth_gen_case0
    , H.testCase "quotrem_knuth matches case0" quotrem_knuth_case0
    , H.testCase "quotrem matches case0" quotrem_case0
    , H.testCase "quotrem matches case1" quotrem_case1
    , H.testCase "quotrem_gen matches case0" quotrem_gen_case0
    , H.testCase "quotrem_gen matches case1" quotrem_gen_case1
    ]
  ]

