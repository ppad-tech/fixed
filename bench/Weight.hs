{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq
import Data.Word.Wider (Wider)
import qualified Data.Word.Wider as W
import qualified Numeric.Montgomery.Secp256k1.Curve as C
import qualified Numeric.Montgomery.Secp256k1.Scalar as S
import Prelude hiding (sqrt, exp)
import Weigh

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith $ do
  num_wider
  cmp
  add
  sub
  mul
  sqr
  inv
  exp
  sqrt
  redc
  retr

num_wider :: Weigh ()
num_wider = wgroup "num_wider" $ do
  func "small" (force :: Wider -> Wider) 2
  func "large" (force :: Wider -> Wider)
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

cmp :: Weigh ()
cmp =
  let !a = 1
      !b = 2
      !c = 2 ^ 255 - 19
  in  wgroup "cmp_vartime" $ do
        func "cmp_vartime: 1 < 2" (W.cmp_vartime a) b
        func "cmp_vartime: 2 < 1" (W.cmp_vartime b) a
        func "cmp_vartime: 2 < 2 ^ 255 - 19" (W.cmp_vartime b) c
        func "cmp_vartime: 2 ^ 255 - 19 < 2" (W.cmp_vartime c) b

add :: Weigh ()
add =
  let !c1 = 1 :: C.Montgomery
      !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s1 = 1 :: S.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  wgroup "add" $ do
        func "curve:  M(1) + M(2)" (C.add c1) c2
        func "curve:  M(1) + M(2 ^ 255 - 19)" (C.add c1) c_big
        func "scalar: M(1) + M(2)" (S.add s1) s2
        func "scalar: M(1) + M(2 ^ 255 - 19)" (S.add s1) s_big

sub :: Weigh ()
sub =
  let !c_max = (2 ^ 255 - 1) :: C.Montgomery
      !c1 = 1 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s_max = (2 ^ 255 - 1) :: S.Montgomery
      !s1 = 1 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  wgroup "sub" $ do
        func "curve:  M(2 ^ 255 - 1) - M(1)" (C.sub c_max) c1
        func "curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)" (C.sub c_max) c_big
        func "scalar: M(2 ^ 255 - 1) - M(1)" (S.sub s_max) s1
        func "scalar: M(2 ^ 255 - 1) - M(2 ^ 255 - 19)" (S.sub s_max) s_big

mul :: Weigh ()
mul =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  wgroup "mul" $ do
        func "curve:  M(2) * M(2)" (C.mul c2) c2
        func "curve:  M(2) * M(2 ^ 255 - 19)" (C.mul c2) c_big
        func "scalar: M(2) * M(2)" (S.mul s2) s2
        func "scalar: M(2) * M(2 ^ 255 - 19)" (S.mul s2) s_big

sqr :: Weigh ()
sqr =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  wgroup "sqr" $ do
        func "curve:  M(2) ^ 2" C.sqr c2
        func "curve:  M(2 ^ 255 - 19) ^ 2" C.sqr c_big
        func "scalar: M(2) ^ 2" S.sqr s2
        func "scalar: M(2 ^ 255 - 19) ^ 2" S.sqr s_big

inv :: Weigh ()
inv =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  wgroup "inv" $ do
        func "curve:  M(2) ^ -1" C.inv c2
        func "curve:  M(2 ^ 255 - 19) ^ -1" C.inv c_big
        func "scalar: M(2) ^ -1" S.inv s2
        func "scalar: M(2 ^ 255 - 19) ^ -1" S.inv s_big

exp :: Weigh ()
exp =
  let !c2 = 2 :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !sma = 2 :: Wider
      !big = (2 ^ 255 - 19) :: Wider
  in  wgroup "exp" $ do
        func "curve:  M(2) ^ 2" (C.exp c2) sma
        func "curve:  M(2) ^ (2 ^ 255 - 19)" (C.exp c2) big
        func "scalar:  M(2) ^ 2" (S.exp s2) sma
        func "scalar:  M(2) ^ (2 ^ 255 - 19)" (S.exp s2) big

sqrt :: Weigh ()
sqrt =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
  in  wgroup "sqrt_vartime" $ do
        func "curve:  sqrt_vartime M(2)" C.sqrt_vartime c2
        func "curve:  sqrt_vartime M(2 ^ 255 - 19)" C.sqrt_vartime c_big

redc :: Weigh ()
redc =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  wgroup "redc" $ do
        func "curve:  REDC(M(2), M(2))" (C.redc c2) c2
        func "curve:  REDC(M(2), M(2 ^ 255 - 19))" (C.redc c2) c_big
        func "scalar: REDC(M(2), M(2))" (S.redc s2) s2
        func "scalar: REDC(M(2), M(2 ^ 255 - 19))" (S.redc s2) s_big

retr :: Weigh ()
retr =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  wgroup "retr" $ do
        func "curve:  RETR(M(2))" C.retr c2
        func "curve:  RETR(M(2 ^ 255 - 19))" C.retr c_big
        func "scalar: RETR(M(2))" S.retr s2
        func "scalar: RETR(M(2 ^ 255 - 19))" S.retr s_big
