{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Numeric.Montgomery.Secp256k1.Curve as C
import qualified Numeric.Montgomery.Secp256k1.Scalar as S
import Weigh

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith $ do
  add
  sub
  mul
  sqr
  inv
  redc
  retr

add :: Weigh ()
add = wgroup "add" $ do
  func "curve:  M(1) + M(2)" (C.add 1) 2
  func "curve:  M(1) + M(2 ^ 255 - 19)" (C.add 1) (2 ^ 255 - 19)
  func "scalar: M(1) + M(2)" (S.add 1) 2
  func "scalar: M(1) + M(2 ^ 255 - 19)" (S.add 1) (2 ^ 255 - 19)

sub :: Weigh ()
sub = wgroup "sub" $ do
  func "curve:  M(2 ^ 255 - 1) - M(1)"
    (C.sub (2 ^ 255 - 1)) 1
  func "curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)"
    (C.sub (2 ^ 255 - 1)) (2 ^ 255 - 19)
  func "scalar: M(2 ^ 255 - 1) - M(1)"
    (S.sub (2 ^ 255 - 1)) 1
  func "scalar: M(2 ^ 255 - 1) - M(2 ^ 255 - 19)"
    (S.sub (2 ^ 255 - 1)) (2 ^ 255 - 19)

mul :: Weigh ()
mul = wgroup "mul" $ do
  func "curve:  M(2) * M(2)" (C.mul 2) 2
  func "curve:  M(2) * M(2 ^ 255 - 19)" (C.mul 2) (2 ^ 255 - 19)
  func "scalar: M(2) * M(2)" (S.mul 2) 2
  func "scalar: M(2) * M(2 ^ 255 - 19)" (S.mul 2) (2 ^ 255 - 19)

sqr :: Weigh ()
sqr = wgroup "sqr" $ do
  func "curve:  M(2) ^ 2" C.sqr 2
  func "curve:  M(2 ^ 255 - 19) ^ 2" C.sqr (2 ^ 255 - 19)
  func "scalar: M(2) ^ 2" S.sqr 2
  func "scalar: M(2 ^ 255 - 19) ^ 2" S.sqr (2 ^ 255 - 19)

inv :: Weigh ()
inv = wgroup "inv" $ do
  func "curve:  M(2) ^ -1" C.inv 2
  func "curve:  M(2 ^ 255 - 19) ^ -1" C.inv (2 ^ 255 - 19)
  func "scalar: M(2) ^ -1" S.inv 2
  func "scalar: M(2 ^ 255 - 19) ^ -1" S.inv (2 ^ 255 - 19)

redc :: Weigh ()
redc = wgroup "redc" $ do
  func "curve:  REDC(M(2), M(2))" (C.redc 2) 2
  func "curve:  REDC(M(2), M(2 ^ 255 - 19))" (C.redc 2) (2 ^ 255 - 19)
  func "scalar: REDC(M(2), M(2))" (S.redc 2) 2
  func "scalar: REDC(M(2), M(2 ^ 255 - 19))" (S.redc 2) (2 ^ 255 - 19)

retr :: Weigh ()
retr = wgroup "retr" $ do
  func "curve:  RETR(M(2))" C.retr 2
  func "curve:  RETR(M(2 ^ 255 - 19))" C.retr (2 ^ 255 - 19)
  func "scalar: RETR(M(2))" S.retr 2
  func "scalar: RETR(M(2 ^ 255 - 19))" S.retr (2 ^ 255 - 19)
