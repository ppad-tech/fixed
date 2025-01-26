# ppad-fixed

A pure (**pre-release**, under construction) implementation of large
fixed-width integers and supporting operations.

## Usage

A sample GHCi session:

```
  > -- import qualified
  > import Data.Word.Extended as W
  >
  > -- use 'to_word256' to convert variable-length integers to Word256
  > let u = W.to_word256 0xFFFFFFFFFF
  > :t u
  u :: W.Word256
  >
  > -- compare values
  > let v = W.to_word256 0xFFFFFF
  > u `W.lt` v
  False
  >
  > -- bitwise operations
  > u `W.or` v      -- == u
  1099511627775
  > u `W.and` v     -- == v
  16777215
  >
  > -- arithmetic operations
  > (u `add` v) `sub` (u `add` v)
  0
  > u `mul` u `mul` u `mul` u `mul` u `mul` u `mul` u
  115779721307862780478962831313825936498328052285500565196053117862789708251135
  >
  > u `div` v
  65536
  >
  > -- modular reduction
  > u `mod` v
  65535
```

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code. Most operations beat out their variable-length Integer variants,
*but*, we have yet to reach parity with the all-important division and
modulo operations.

There are a few ways I can imagine this being improved:

* Extremely careful use of Data.Primitive.PrimArray can likely beat
  the pure code that currently implements most of the library. A
  single allocation of a maximum 21-long mutable PrimArray is not
  terribly expensive. In particular finding a remainder, e.g. for
  modulo, requires comparatively few expensive writes.

  We require, for cases like modular multplication, at most 8 elements
  for the quotient, 9 elements for the dividend/normalized dividend
  (i.e. reusing the same area for both), and 8 elements for the
  divisor/normalized divisor (ditto), assuming we stick with Knuth's division
  algorithm. In other situations, e.g. division or modulo, we need less.

* Avoid unnecessarily calculating the quotient or remainder if only one
  of the two is desired.

* It's possible that manually writing internals using primitives (think
  MagicHash, UnboxedTuples, GHC.Exts) could yield an improvement; I've
  noticed that GHC doesn't always unbox everything automatically. This
  is possibly due to register contention, though, so it could also
  potentially degrade performance.

* An alternative division algorithm might be worth looking into, if it
  proves to be impossible to squeeze more out of Knuth's.

Current benchmark figures on my mid-2020 MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking baseline comparison/add (baseline)
  time                 24.60 ns   (24.42 ns .. 24.78 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 24.69 ns   (24.56 ns .. 24.81 ns)
  std dev              438.0 ps   (373.9 ps .. 550.6 ps)
  variance introduced by outliers: 25% (moderately inflated)

  benchmarking baseline comparison/add
  time                 13.72 ns   (13.46 ns .. 13.97 ns)
                       0.998 R²   (0.998 R² .. 0.999 R²)
  mean                 13.53 ns   (13.39 ns .. 13.70 ns)
  std dev              513.9 ps   (413.5 ps .. 660.9 ps)
  variance introduced by outliers: 61% (severely inflated)

  benchmarking baseline comparison/sub (baseline)
  time                 31.59 ns   (31.34 ns .. 31.93 ns)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 32.32 ns   (31.86 ns .. 33.06 ns)
  std dev              1.987 ns   (1.383 ns .. 2.990 ns)
  variance introduced by outliers: 80% (severely inflated)

  benchmarking baseline comparison/sub
  time                 12.34 ns   (12.16 ns .. 12.53 ns)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 12.39 ns   (12.26 ns .. 12.53 ns)
  std dev              446.6 ps   (366.2 ps .. 547.7 ps)
  variance introduced by outliers: 59% (severely inflated)

  benchmarking baseline comparison/mul (baseline)
  time                 48.55 ns   (47.05 ns .. 51.12 ns)
                       0.990 R²   (0.973 R² .. 0.999 R²)
  mean                 47.76 ns   (47.02 ns .. 49.82 ns)
  std dev              3.797 ns   (1.911 ns .. 7.726 ns)
  variance introduced by outliers: 87% (severely inflated)

  benchmarking baseline comparison/mul
  time                 30.70 ns   (30.35 ns .. 31.03 ns)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 30.81 ns   (30.46 ns .. 31.23 ns)
  std dev              1.286 ns   (991.8 ps .. 1.903 ns)
  variance introduced by outliers: 65% (severely inflated)

  benchmarking baseline comparison/div (baseline)
  time                 81.75 ns   (80.71 ns .. 82.84 ns)
                       0.998 R²   (0.998 R² .. 0.999 R²)
  mean                 82.12 ns   (81.11 ns .. 83.44 ns)
  std dev              3.844 ns   (3.209 ns .. 4.565 ns)
  variance introduced by outliers: 68% (severely inflated)

  benchmarking baseline comparison/div
  time                 182.2 ns   (178.8 ns .. 185.3 ns)
                       0.997 R²   (0.996 R² .. 0.998 R²)
  mean                 182.9 ns   (179.3 ns .. 186.3 ns)
  std dev              11.07 ns   (9.342 ns .. 13.50 ns)
  variance introduced by outliers: 77% (severely inflated)
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
