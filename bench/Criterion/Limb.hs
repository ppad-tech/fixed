module Criterion.Limb (
    benches
  ) where

import Criterion.Main
import qualified Data.Word.Limb as Limb
import Prelude hiding (recip)

benches :: Benchmark
benches = bgroup "limb arithmetic" [
    quot_big
  , quot_small
  , recip
  ]

quot_big :: Benchmark
quot_big = bench "wide quot (big)" $
  nf (Limb.quot 4294967294 32 4294967293) 32

quot_small :: Benchmark
quot_small = bench "wide quot (small)" $
  nf (Limb.quot 4294967294 32 2) 2

recip :: Benchmark
recip = bench "wide recip" $
  nf Limb.recip 18446744073709551606

