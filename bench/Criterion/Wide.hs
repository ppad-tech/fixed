module Criterion.Wide (
    benches
  ) where

import Criterion.Main
import qualified Data.Word.Wide as Wide
import Prelude hiding (recip)

benches :: Benchmark
benches = bgroup "wide arithmetic" [
    quotrem_by1_bench
  , shr_bench_small
  , shr_bench_big
  ]

quotrem_by1_bench :: Benchmark
quotrem_by1_bench = bench "wide quotrem_by1" $
  nf (Wide.quotrem_by1 (Wide.wide 4294967294 32)) 18446744073709551606

_quotrem_by1_bench :: Benchmark
_quotrem_by1_bench = bench "wide _quotrem_by1" $
  nf (Wide._quotrem_by1 (Wide.wide 4294967294 32)) 18446744073709551606

shr_bench_small :: Benchmark
shr_bench_small = bench "wide shr (small)" $
  nf (Wide.shr (Wide.wide maxBound maxBound)) 1

shr_bench_big :: Benchmark
shr_bench_big = bench "wide shr (big)" $
  nf (Wide.shr (Wide.wide maxBound maxBound)) 127
