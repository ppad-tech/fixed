{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Word.Wider (Wider)
import qualified Numeric.Montgomery.Secp256k1.Curve as C
import qualified Numeric.Montgomery.Secp256k1.Scalar as S
import Criterion.Main
import Prelude hiding (exp, sqrt)

main :: IO ()
main = defaultMain [
    add
  , sub
  , mul
  , sqr
  , inv
  , exp
  , exp_vartime
  , sqrt
  , redc
  , retr
  ]

add :: Benchmark
add =
  let !c1 = 1 :: C.Montgomery
      !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s1 = 1 :: S.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  bgroup "add" [
          bench "curve:  M(1) + M(2)" $ nf (C.add c1) c2
        , bench "curve:  M(1) + M(2 ^ 255 - 19)" $ nf (C.add c1) c_big
        , bench "scalar: M(1) + M(2)" $ nf (S.add s1) s2
        , bench "scalar: M(1) + M(2 ^ 255 - 19)" $ nf (S.add s1) s_big
        ]

sub :: Benchmark
sub =
  let !c_max = (2 ^ 255 - 1) :: C.Montgomery
      !c1 = 1 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s_max = (2 ^ 255 - 1) :: S.Montgomery
      !s1 = 1 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  bgroup "sub" [
          bench "curve:  M(2 ^ 255 - 1) - M(1)" $ nf (C.sub c_max) c1
        , bench "curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)" $
            nf (C.sub c_max) c_big
        , bench "scalar: M(2 ^ 255 - 1) - M(1)" $ nf (S.sub s_max) s1
        , bench "scalar: M(2 ^ 255 - 1) - M(2 ^ 255 - 19)" $
            nf (S.sub s_max) s_big
        ]

mul :: Benchmark
mul =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  bgroup "mul" [
          bench "curve:  M(2) * M(2)" $ nf (C.mul c2) c2
        , bench "curve:  M(2) * M(2 ^ 255 - 19)" $ nf (C.mul c2) c_big
        , bench "scalar: M(2) * M(2)" $ nf (S.mul s2) s2
        , bench "scalar: M(2) * M(2 ^ 255 - 19)" $ nf (S.mul s2) s_big
        ]

sqr :: Benchmark
sqr =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  bgroup "sqr" [
          bench "curve:  M(2) ^ 2" $ nf C.sqr c2
        , bench "curve:  M(2 ^ 255 - 19) ^ 2" $ nf C.sqr c_big
        , bench "scalar: M(2) ^ 2" $ nf S.sqr s2
        , bench "scalar: M(2 ^ 255 - 19) ^ 2" $ nf S.sqr s_big
        ]

inv :: Benchmark
inv =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  bgroup "inv" [
          bench "curve:  M(2) ^ -1" $ nf C.inv c2
        , bench "curve:  M(2 ^ 255 - 19) ^ -1" $ nf C.inv c_big
        , bench "scalar: M(2) ^ -1" $ nf S.inv s2
        , bench "scalar: M(2 ^ 255 - 19) ^ -1" $ nf S.inv s_big
        ]

sqrt :: Benchmark
sqrt =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
  in  bgroup "sqrt" [
          bench "curve:  sqrt M(2)" $ nf C.sqrt c2
        , bench "curve:  sqrt M(2 ^ 255 - 19)" $ nf C.sqrt c_big
        ]

exp :: Benchmark
exp =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
      !e2 = 2 :: Wider
      !e_big = (2 ^ 255 - 19) :: Wider
  in  bgroup "exp" [
          bench "curve:  M(2) ^ 2" $ nf (C.exp c2) e2
        , bench "curve:  M(2 ^ 255 - 19) ^ (2 ^ 255 - 19)" $
            nf (C.exp c_big) e_big
        , bench "scalar: M(2) ^ 2" $ nf (S.exp s2) e2
        , bench "scalar: M(2 ^ 255 - 19) ^ (2 ^ 255 - 19)" $
            nf (S.exp s_big) e_big
        ]

exp_vartime :: Benchmark
exp_vartime =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
      !e2 = 2 :: Wider
      !e_big = (2 ^ 255 - 19) :: Wider
  in  bgroup "exp_vartime" [
          bench "curve:  M(2) ^ 2" $ nf (C.exp_vartime c2) e2
        , bench "curve:  M(2 ^ 255 - 19) ^ (2 ^ 255 - 19)" $
            nf (C.exp_vartime c_big) e_big
        , bench "scalar: M(2) ^ 2" $ nf (S.exp_vartime s2) e2
        , bench "scalar: M(2 ^ 255 - 19) ^ (2 ^ 255 - 19)" $
            nf (S.exp_vartime s_big) e_big
        ]

redc :: Benchmark
redc =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  bgroup "redc" [
          bench "curve:  REDC(M(2), M(2))" $ nf (C.redc c2) c2
        , bench "curve:  REDC(M(2), M(2 ^ 255 - 19))" $ nf (C.redc c2) c_big
        , bench "scalar: REDC(M(2), M(2))" $ nf (S.redc s2) s2
        , bench "scalar: REDC(M(2), M(2 ^ 255 - 19))" $ nf (S.redc s2) s_big
        ]

retr :: Benchmark
retr =
  let !c2 = 2 :: C.Montgomery
      !c_big = (2 ^ 255 - 19) :: C.Montgomery
      !s2 = 2 :: S.Montgomery
      !s_big = (2 ^ 255 - 19) :: S.Montgomery
  in  bgroup "retr" [
          bench "curve:  RETR(M(2))" $ nf C.retr c2
        , bench "curve:  RETR(M(2 ^ 255 - 19))" $ nf C.retr c_big
        , bench "scalar: RETR(M(2))" $ nf S.retr s2
        , bench "scalar: RETR(M(2 ^ 255 - 19))" $ nf S.retr s_big
        ]
