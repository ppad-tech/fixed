# ppad-fixed

A pure (**pre-release**, under construction,
not-yet-guaranteed-constant-time) high-performance implementation
of large fixed-width integers and supporting operations, including
Montgomery-form arithmetic on domains related to the the elliptic curve
secp256k1.

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
time                 6.890 ns   (6.882 ns .. 6.906 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.889 ns   (6.885 ns .. 6.897 ns)
std dev              16.73 ps   (8.753 ps .. 30.75 ps)

benchmarking sub/curve:  M(2 ^ 255 - 1) - M(2 ^ 255 - 19)
time                 6.876 ns   (6.872 ns .. 6.883 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.877 ns   (6.874 ns .. 6.881 ns)
std dev              11.60 ps   (7.865 ps .. 18.34 ps)

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
time                 7.004 μs   (6.988 μs .. 7.030 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.027 μs   (7.013 μs .. 7.047 μs)
std dev              57.73 ns   (50.57 ns .. 74.05 ns)
```

The library can be used either via a boxed or unboxed API. Unboxed
functions, suffixed by magic hashes, do not allocate. Boxed functions
allocate only for top-level data constructors used when boxing results
-- any internal arithmetic is entirely unboxed.

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
```

Note that you can compile with GHC's LLVM backend and filter on specific
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

This library is more or less a Haskell translation of the golang
[uint256](https://github.com/holiman/uint256) library.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
