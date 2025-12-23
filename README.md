# ppad-fixed

[![](https://img.shields.io/hackage/v/ppad-fixed?color=blue)](https://hackage.haskell.org/package/ppad-fixed)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-fixed-lightblue)](https://docs.ppad.tech/fixed)

A pure high-performance implementation of large fixed-width integers
and supporting constant-time operations, including Montgomery-form
arithmetic on domains related to the the elliptic curve secp256k1.

## Usage

A sample GHCi session:

```
  > -- import qualified
  > import qualified Numeric.Montgomery.Secp256k1.Curve as C
  >
  > let one = 1 :: C.Montgomery
  > one
  1
  > putStrLn (C.render one)
  (4294968273, 0, 0, 0)
  >
  > let two = one + one
  > putStrLn (C.render two)
  (8589936546, 0, 0, 0)
  >
  > let big = 2 ^ (128 :: Int) :: C.Montgomery
  > big
  340282366920938463463374607431768211456
  > putStrLn (C.render big)
  (0, 0, 4294968273, 0)
  >
  > let inv = C.inv big
  > inv
  85349562743316995932995116683053049354367560536510302240860302699983992117553
  > putStrLn (C.render inv)
  (0, 0, 1, 0)
  >
  > inv * big
  1
```

## Performance

The aim is best-in-class performance for pure Haskell code.

Current benchmark figures on my M4 Silicon MacBook Air look like:

```
benchmarking add/curve:  M(1) + M(2 ^ 255 - 19)
time                 5.399 ns   (5.359 ns .. 5.442 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.389 ns   (5.371 ns .. 5.426 ns)
std dev              82.34 ps   (54.62 ps .. 128.7 ps)
variance introduced by outliers: 21% (moderately inflated)

benchmarking sub/curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)
time                 5.226 ns   (5.217 ns .. 5.241 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.249 ns   (5.230 ns .. 5.287 ns)
std dev              84.60 ps   (54.00 ps .. 126.7 ps)
variance introduced by outliers: 23% (moderately inflated)

benchmarking mul/curve:  M(2) * M(2 ^ 255 - 19)
time                 14.78 ns   (14.75 ns .. 14.81 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.82 ns   (14.79 ns .. 14.85 ns)
std dev              99.51 ps   (84.22 ps .. 118.2 ps)

benchmarking sqr/curve:  M(2 ^ 255 - 19) ^ 2
time                 14.23 ns   (14.20 ns .. 14.27 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.26 ns   (14.23 ns .. 14.29 ns)
std dev              114.3 ps   (84.98 ps .. 181.1 ps)

benchmarking inv/curve:  M(2 ^ 255 - 19) ^ -1
time                 6.936 μs   (6.911 μs .. 6.959 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.898 μs   (6.885 μs .. 6.911 μs)
std dev              44.83 ns   (35.58 ns .. 56.92 ns)

benchmarking exp/curve:  M(2 ^ 255 - 19) ^ (2 ^ 255 - 19)
time                 5.200 μs   (5.194 μs .. 5.205 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.192 μs   (5.188 μs .. 5.197 μs)
std dev              15.58 ns   (11.38 ns .. 20.86 ns)

benchmarking sqrt/curve:  sqrt M(2 ^ 255 - 19)
time                 6.882 μs   (6.876 μs .. 6.890 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.893 μs   (6.870 μs .. 6.902 μs)
std dev              47.48 ns   (25.79 ns .. 100.5 ns)
```

The library can be used either via a boxed or unboxed API. Functions in
the unboxed API, suffixed by magic hashes, do not allocate. Functions in
the boxed API allocate only for top-level data constructors used when
boxing results -- any internal arithmetic is entirely unboxed.

For example, a boxed 'Wide' word, defined as:

``` haskell
  data Wide = Wide !(# Limb, Limb #)
```

will allocate precisely three machine words -- one for the constructor,
and one for each 'Limb' (so, 24 bytes on a 64-bit machine). A 'Wider'
word, defined as:

``` haskell
  data Wider = Wider !(# Limb, Limb, Limb, Limb #)
```

will allocate five machine words (40 bytes) in turn.

Thus, the allocation benchmark story for 256-bit Montgomery arithmetic
using the boxed API looks like:

```
add

  Case                            Allocated  GCs
  curve:  M(1) + M(2)                    40    0
  curve:  M(1) + M(2 ^ 255 - 19)         40    0

sub

  Case                                      Allocated  GCs
  curve:  M(2 ^ 255 - 1) - M(1)                    40    0
  curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)         40    0

mul

  Case                            Allocated  GCs
  curve:  M(2) * M(2)                    40    0
  curve:  M(2) * M(2 ^ 255 - 19)         40    0

sqr

  Case                         Allocated  GCs
  curve:  M(2) ^ 2                    40    0
  curve:  M(2 ^ 255 - 19) ^ 2         40    0

inv

  Case                          Allocated  GCs
  curve:  M(2) ^ -1                    40    0
  curve:  M(2 ^ 255 - 19) ^ -1         40    0

exp

  Case                            Allocated  GCs
  curve:  M(2) ^ 2                       40    0
  curve:  M(2) ^ (2 ^ 255 - 19)          40    0

sqrt

  Case                          Allocated  GCs
  curve:  sqrt M(2)                    56    0
  curve:  sqrt M(2 ^ 255 - 19)         56    0
```

Note that 'sqrt' for example allocates 16 additional bytes as it returns
a value of type 'Maybe Montgomery', which involves using either a Just
or Nothing constructor (the analogous function in the unboxed API,
'sqrt#', avoids allocation by returning an unboxed sum).

You can compile with GHC's LLVM backend and filter on specific
benchmarks via e.g.:

```
$ cabal bench ppad-fixed:benchmark:fixed-bench \
    --ghc-options="-fllvm -O2" \
    --benchmark-options="--match prefix inv"
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be challenging to achieve.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-fixed
```

to get a REPL for the main library.

## Attribution

This library is more or less a Haskell translation of (parts of) the
Rust [crypto-bigint](https://github.com/RustCrypto/crypto-bigint)
library, having initially started as a port of the golang
[uint256](https://github.com/holiman/uint256) library.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
