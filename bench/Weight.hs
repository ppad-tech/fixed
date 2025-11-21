{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Word.Wider as W
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
  func "curve:  M(1) + M(2)"
    (C.add C.one)
    (C.to (W.to 2))
  func "curve:  M(1) + M(2 ^ 255 - 19)"
    (C.add C.one)
    (C.to (W.to (2 ^ 255 - 19)))
  func "scalar: M(1) + M(2)"
    (S.add S.one)
    (S.to (W.to 2))
  func "scalar: M(1) + M(2 ^ 255 - 19)"
    (S.add S.one)
    (S.to (W.to (2 ^ 255 - 19)))

sub :: Weigh ()
sub = wgroup "sub" $ do
  func "curve:  M(2 ^ 255 - 1) - M(1)"
    (C.sub (C.to (W.to (2 ^ 255 - 1))))
    C.one
  func "curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)"
    (C.sub (C.to (W.to (2 ^ 255 - 1))))
    (C.to (W.to (2 ^ 255 - 19)))
  func "scalar: M(2 ^ 255 - 1) - M(1)"
    (S.sub (S.to (W.to (2 ^ 255 - 1))))
    S.one
  func "scalar: M(2 ^ 255 - 1) - M(2 ^ 255 - 19)"
    (S.sub (S.to (W.to (2 ^ 255 - 1))))
    (S.to (W.to (2 ^ 255 - 19)))

mul :: Weigh ()
mul = wgroup "mul" $ do
  func "curve:  M(2) * M(2)"
    (C.mul (C.to (W.to 2)))
    (C.to (W.to 2))
  func "curve:  M(2) * M(2 ^ 255 - 19)"
    (C.mul (C.to (W.to 2)))
    (C.to (W.to (2 ^ 255 - 19)))
  func "scalar: M(2) * M(2)"
    (S.mul (S.to (W.to 2)))
    (S.to (W.to 2))
  func "scalar: M(2) * M(2 ^ 255 - 19)"
    (S.mul (S.to (W.to 2)))
    (S.to (W.to (2 ^ 255 - 19)))

sqr :: Weigh ()
sqr = wgroup "sqr" $ do
  func "curve:  M(2) ^ 2" C.sqr
    (C.to (W.to 2))
  func "curve:  M(2 ^ 255 - 19) ^ 2" C.sqr
    (C.to (W.to (2 ^ 255 - 19)))
  func "scalar: M(2) ^ 2" S.sqr
    (S.to (W.to 2))
  func "scalar: M(2 ^ 255 - 19) ^ 2" S.sqr
    (S.to (W.to (2 ^ 255 - 19)))

inv :: Weigh ()
inv = wgroup "inv" $ do
  func "curve:  M(2) ^ -1" C.inv
    (C.to (W.to 2))
  func "curve:  M(2 ^ 255 - 19) ^ -1" C.inv
    (C.to (W.to (2 ^ 255 - 19)))
  func "scalar: M(2) ^ -1" S.inv
    (S.to (W.to 2))
  func "scalar: M(2 ^ 255 - 19) ^ -1" S.inv
    (S.to (W.to (2 ^ 255 - 19)))

redc :: Weigh ()
redc = wgroup "redc" $ do
  func "curve:  REDC(M(2), M(2))"
    (C.redc (C.to (W.to 2)))
    (C.to (W.to 2))
  func "curve:  REDC(M(2), M(2 ^ 255 - 19))"
    (C.redc (C.to (W.to 2)))
    (C.to (W.to (2 ^ 255 - 19)))
  func "scalar: REDC(M(2), M(2))"
    (S.redc (S.to (W.to 2)))
    (S.to (W.to 2))
  func "scalar: REDC(M(2), M(2 ^ 255 - 19))"
    (S.redc (S.to (W.to 2)))
    (S.to (W.to (2 ^ 255 - 19)))

retr :: Weigh ()
retr = wgroup "retr" $ do
  func "curve:  RETR(M(2))" C.retr
    (C.to (W.to 2))
  func "curve:  RETR(M(2 ^ 255 - 19))" C.retr
    (C.to (W.to (2 ^ 255 - 19)))
  func "scalar: RETR(M(2))" S.retr
    (S.to (W.to 2))
  func "scalar: RETR(M(2 ^ 255 - 19))" S.retr
    (S.to (W.to (2 ^ 255 - 19)))
