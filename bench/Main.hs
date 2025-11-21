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
    bench "curve:  M(2) ^ 2" $ nf C.sqr
      (C.to (W.to 2))
  , bench "curve:  M(2 ^ 255 - 19) ^ 2" $ nf C.sqr
      (C.to (W.to (2 ^ 255 - 19)))
  , bench "scalar: M(2) ^ 2" $ nf S.sqr
      (S.to (W.to 2))
  , bench "scalar: M(2 ^ 255 - 19) ^ 2" $ nf S.sqr
      (S.to (W.to (2 ^ 255 - 19)))
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

-- redc :: Weigh ()
-- redc = wgroup "redc" $ do
--   func "curve:  REDC(M(2), M(2))"
--     (C.redc (C.to (W.to 2)))
--     (C.to (W.to 2))
--   func "curve:  REDC(M(2), M(2 ^ 255 - 19))"
--     (C.redc (C.to (W.to 2)))
--     (C.to (W.to (2 ^ 255 - 19)))
--   func "scalar: REDC(M(2), M(2))"
--     (S.redc (S.to (W.to 2)))
--     (S.to (W.to 2))
--   func "scalar: REDC(M(2), M(2 ^ 255 - 19))"
--     (S.redc (S.to (W.to 2)))
--     (S.to (W.to (2 ^ 255 - 19)))
--
-- retr :: Weigh ()
-- retr = wgroup "retr" $ do
--   func "curve:  RETR(M(2))" C.retr
--     (C.to (W.to 2))
--   func "curve:  RETR(M(2 ^ 255 - 19))" C.retr
--     (C.to (W.to (2 ^ 255 - 19)))
--   func "scalar: RETR(M(2))" S.retr
--     (S.to (W.to 2))
--   func "scalar: RETR(M(2 ^ 255 - 19))" S.retr
--     (S.to (W.to (2 ^ 255 - 19)))



