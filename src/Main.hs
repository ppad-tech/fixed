{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Word.Extended

main :: IO ()
main = do
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

  let foo = quotrem_gen u d
  print foo

