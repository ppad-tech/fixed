{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Primitive.PrimArray as PA
import Data.Word.Extended

main :: IO ()
main = do
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

  quo <- PA.newPrimArray 5
  let go !j !acc
        | j == 10000 = pure ()
        | otherwise = do
            PA.setPrimArray quo 0 5 0
            w <- quotrem quo u d
            go (succ j) w
  r <- go 0 zero
  q <- PA.unsafeFreezePrimArray quo
  print r
  print q

