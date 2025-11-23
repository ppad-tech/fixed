{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Word.Wider as W
import qualified Numeric.Montgomery.Secp256k1.Curve as C
import qualified Numeric.Montgomery.Secp256k1.Scalar as S
import Criterion.Main

main :: IO ()
main = defaultMain [
    add
  , sub
  , mul
  , sqr
  , inv
  , redc
  , retr
  ]

add :: Benchmark
add = bgroup "add" [
    bench "curve:  M(1) + M(2)" $ nf
      (C.add C.one)
      (C.to (W.to 2))
  , bench "curve:  M(1) + M(2 ^ 255 - 19)" $ nf
      (C.add C.one)
      (C.to (W.to (2 ^ 255 - 19)))
  , bench "scalar: M(1) + M(2)" $ nf
      (S.add S.one)
      (S.to (W.to 2))
  , bench "scalar: M(1) + M(2 ^ 255 - 19)" $ nf
      (S.add S.one)
      (S.to (W.to (2 ^ 255 - 19)))
  ]

sub :: Benchmark
sub = bgroup "sub" [
    bench "curve:  M(2 ^ 255 - 1) - M(1)" $ nf
      (C.sub (C.to (W.to (2 ^ 255 - 1))))
      C.one
  , bench "curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)" $ nf
      (C.sub (C.to (W.to (2 ^ 255 - 1))))
      (C.to (W.to (2 ^ 255 - 19)))
  , bench "scalar: M(2 ^ 255 - 1) - M(1)" $ nf
      (S.sub (S.to (W.to (2 ^ 255 - 1))))
      S.one
  , bench "scalar: M(2 ^ 255 - 1) - M(2 ^ 255 - 19)" $ nf
      (S.sub (S.to (W.to (2 ^ 255 - 1))))
      (S.to (W.to (2 ^ 255 - 19)))
  ]

mul :: Benchmark
mul = bgroup "mul" [
    bench "curve:  M(2) * M(2)" $ nf
      (C.mul (C.to (W.to 2)))
      (C.to (W.to 2))
  , bench "curve:  M(2) * M(2 ^ 255 - 19)" $ nf
      (C.mul (C.to (W.to 2)))
      (C.to (W.to (2 ^ 255 - 19)))
  , bench "scalar: M(2) * M(2)" $ nf
      (S.mul (S.to (W.to 2)))
      (S.to (W.to 2))
  , bench "scalar: M(2) * M(2 ^ 255 - 19)" $ nf
      (S.mul (S.to (W.to 2)))
      (S.to (W.to (2 ^ 255 - 19)))
  ]

sqr :: Benchmark
sqr = bgroup "sqr" [
    bench "curve:  M(2) ^ 2" $ nf C.sqr 2
  , bench "curve:  M(2 ^ 255 - 19) ^ 2" $ nf C.sqr (2 ^ 255 - 19)
  , bench "scalar: M(2) ^ 2" $ nf S.sqr 2
  , bench "scalar: M(2 ^ 255 - 19) ^ 2" $ nf S.sqr (2 ^ 255 - 19)
  ]

inv :: Benchmark
inv = bgroup "inv" [
    bench "curve:  M(2) ^ -1" $ nf C.inv
      (C.to (W.to 2))
  , bench "curve:  M(2 ^ 255 - 19) ^ -1" $ nf C.inv
      (C.to (W.to (2 ^ 255 - 19)))
  , bench "scalar: M(2) ^ -1" $ nf S.inv
      (S.to (W.to 2))
  , bench "scalar: M(2 ^ 255 - 19) ^ -1" $ nf S.inv
      (S.to (W.to (2 ^ 255 - 19)))
  ]

redc :: Benchmark
redc = bgroup "redc" [
     bench "curve:  REDC(M(2), M(2))" $ nf (C.redc 2)
       2
  ,  bench "curve:  REDC(M(2), M(2 ^ 255 - 19))" $ nf (C.redc 2)
       (2 ^ 255 - 19)
  ,  bench "scalar: REDC(M(2), M(2))" $ nf (S.redc 2)
       2
  ,  bench "scalar: REDC(M(2), M(2 ^ 255 - 19))" $ nf (S.redc 2)
       (2 ^ 255 - 19)
  ]

retr :: Benchmark
retr = bgroup "retr" [
    bench "curve:  RETR(M(2))" $ nf C.retr
      2
  , bench "curve:  RETR(M(2 ^ 255 - 19))" $ nf C.retr
      (2 ^ 255 - 19)
  , bench "scalar: RETR(M(2))" $ nf S.retr
      2
  , bench "scalar: RETR(M(2 ^ 255 - 19))" $ nf S.retr
      (2 ^ 255 - 19)
  ]

