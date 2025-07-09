{-# LANGUAGE MagicHash #-}

module Criterion.Choice (
    choice_utils
  ) where

import Criterion.Main
import qualified Data.Choice as C
import GHC.Exts

-- XX some of these are so fast that they're difficult to benchmark naively
--    like this; performance is dominated by function call overhead and such.
--    likely it would be better to benchmark them executed repeatedly in
--    a loop

choice_utils :: Benchmark
choice_utils = bgroup "choice utilities" [
  ]

