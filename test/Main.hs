{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as Q


--division :: TestTree
--division = testGroup "division" [
--    Q.testProperty "division matches" $
--      Q.withMaxSuccess 1000 div_matches
--  , Q.testProperty "mod matches" $
--      Q.withMaxSuccess 1000 mod_matches
--  ]
--
--main :: IO ()
--main = defaultMain $ testGroup "ppad-fixed" [
--    testGroup "property tests" [
--      add_sub
--    , multiplication
--    , division
--    ]
--  , testGroup "unit tests" [
--      H.testCase "quotrem_r matches case0" quotrem_r_case0
--    , H.testCase "quotrem_r matches case1" quotrem_r_case1
--    , H.testCase "quotrem_r matches case2" quotrem_r_case2
--    , H.testCase "recip_2by1 matches case0" recip_2by1_case0
--    , H.testCase "recip_2by1 matches case1" recip_2by1_case1
--    , H.testCase "quotrem_2by1 matches case0" quotrem_2by1_case0
--    , H.testCase "quotrem_by1 matches case0" quotrem_by1_case0
--    ]
--  , W.tests
--  ]
--
