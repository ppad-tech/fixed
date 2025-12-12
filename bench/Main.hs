{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
  , sqrt
  , redc
  , retr
  ]

add :: Benchmark
add = bgroup "add" [
    bench "curve:  M(1) + M(2)" $ nf (C.add 1) 2
  , bench "curve:  M(1) + M(2 ^ 255 - 19)" $ nf (C.add 1) (2 ^ 255 - 19)
  , bench "scalar: M(1) + M(2)" $ nf (S.add 1) 2
  , bench "scalar: M(1) + M(2 ^ 255 - 19)" $ nf (S.add 1) (2 ^ 255 - 19)
  ]

sub :: Benchmark
sub = bgroup "sub" [
    bench "curve:  M(2 ^ 255 - 1) - M(1)" $ nf
      (C.sub (2 ^ 255 - 1))
      1
  , bench "curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)" $ nf
      (C.sub (2 ^ 255 - 1))
      (2 ^ 255 - 19)
  , bench "scalar: M(2 ^ 255 - 1) - M(1)" $ nf
      (S.sub (2 ^ 255 - 1))
      1
  , bench "scalar: M(2 ^ 255 - 1) - M(2 ^ 255 - 19)" $ nf
      (S.sub (2 ^ 255 - 1))
      (2 ^ 255 - 19)
  ]

mul :: Benchmark
mul = bgroup "mul" [
    bench "curve:  M(2) * M(2)" $ nf (C.mul 2) 2
  , bench "curve:  M(2) * M(2 ^ 255 - 19)" $ nf (C.mul 2) (2 ^ 255 - 19)
  , bench "scalar: M(2) * M(2)" $ nf (S.mul 2) 2
  , bench "scalar: M(2) * M(2 ^ 255 - 19)" $ nf (S.mul 2) (2 ^ 255 - 19)
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
    bench "curve:  M(2) ^ -1" $ nf C.inv 2
  , bench "curve:  M(2 ^ 255 - 19) ^ -1" $ nf C.inv (2 ^ 255 - 19)
  , bench "scalar: M(2) ^ -1" $ nf S.inv 2
  , bench "scalar: M(2 ^ 255 - 19) ^ -1" $ nf S.inv (2 ^ 255 - 19)
  ]

sqrt :: Benchmark
sqrt = bgroup "sqrt" [
    bench "curve:  sqrt M(2)" $ nf C.sqrt 2
  , bench "curve:  sqrt M(2 ^ 255 - 19)" $ nf C.sqrt (2 ^ 255 - 19)
  ]

exp :: Benchmark
exp = bgroup "exp" [
    bench "curve:  M(2) ^ 2" $ nf C.exp 2
  , bench "curve:  M(2 ^ 255 - 19) ^ 2" $ nf C.exp (2 ^ 255 - 19)
  , bench "scalar: M(2) ^ 2" $ nf S.exp 2
  , bench "scalar: M(2 ^ 255 - 19) ^ 2" $ nf S.exp (2 ^ 255 - 19)
  ]

redc :: Benchmark
redc = bgroup "redc" [
     bench "curve:  REDC(M(2), M(2))" $ nf (C.redc 2) 2
  ,  bench "curve:  REDC(M(2), M(2 ^ 255 - 19))" $ nf (C.redc 2) (2 ^ 255 - 19)
  ,  bench "scalar: REDC(M(2), M(2))" $ nf (S.redc 2) 2
  ,  bench "scalar: REDC(M(2), M(2 ^ 255 - 19))" $ nf (S.redc 2) (2 ^ 255 - 19)
  ]

retr :: Benchmark
retr = bgroup "retr" [
    bench "curve:  RETR(M(2))" $ nf C.retr 2
  , bench "curve:  RETR(M(2 ^ 255 - 19))" $ nf C.retr (2 ^ 255 - 19)
  , bench "scalar: RETR(M(2))" $ nf S.retr 2
  , bench "scalar: RETR(M(2 ^ 255 - 19))" $ nf S.retr (2 ^ 255 - 19)
  ]

