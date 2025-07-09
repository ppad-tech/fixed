module Criterion.Wide (
    benches
  ) where

import Criterion.Main
import qualified Data.Word.Wide as Wide
import Prelude hiding (recip)

benches :: Benchmark
benches = bgroup "wide arithmetic" [
    div1by1_big
  , div1by1_small
  , recip
  , div2by1_bench
  , quotrem2by1_bench
  ]

div1by1_big :: Benchmark
div1by1_big = bench "wide div1by1 (big)" $
  nf (Wide.div1by1 4294967294 32 4294967293) 32

div1by1_small :: Benchmark
div1by1_small = bench "wide div1by1 (small)" $
  nf (Wide.div1by1 4294967294 32 2) 2

recip :: Benchmark
recip = bench "wide recip" $
  nf Wide.recip 18446744073709551606

div2by1_bench :: Benchmark
div2by1_bench = bench "wide div2by1" $
  nf (Wide.div2by1 (Wide.wide 4294967294 32)) 18446744073709551606

quotrem2by1_bench :: Benchmark
quotrem2by1_bench = bench "wide quotrem2by1" $
  nf (Wide.quotrem2by1 (Wide.wide 4294967294 32)) 18446744073709551606

