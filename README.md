# ppad-fixed

A pure (**pre-release**, under construction, not-yet-constant-time)
implementation of large fixed-width integers and supporting operations.

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

Current benchmark figures on my mid-2020 MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking addition & subtraction/add
  time                 11.20 ns   (11.11 ns .. 11.30 ns)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 11.47 ns   (11.32 ns .. 11.75 ns)
  std dev              679.6 ps   (380.7 ps .. 1.038 ns)
  variance introduced by outliers: 80% (severely inflated)

  benchmarking addition & subtraction/add (baseline)
  time                 25.09 ns   (24.36 ns .. 26.00 ns)
                       0.991 R²   (0.981 R² .. 0.998 R²)
  mean                 24.81 ns   (24.27 ns .. 25.85 ns)
  std dev              2.408 ns   (1.289 ns .. 4.439 ns)
  variance introduced by outliers: 91% (severely inflated)

  benchmarking addition & subtraction/sub
  time                 11.51 ns   (11.42 ns .. 11.63 ns)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 11.66 ns   (11.54 ns .. 11.81 ns)
  std dev              475.9 ps   (350.7 ps .. 642.7 ps)
  variance introduced by outliers: 65% (severely inflated)

  benchmarking addition & subtraction/sub (baseline)
  time                 31.93 ns   (31.71 ns .. 32.13 ns)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 31.65 ns   (31.39 ns .. 31.89 ns)
  std dev              841.0 ps   (710.8 ps .. 1.002 ns)
  variance introduced by outliers: 42% (moderately inflated)

  benchmarking multiplication/mul
  time                 29.41 ns   (29.06 ns .. 29.84 ns)
                       0.995 R²   (0.988 R² .. 0.999 R²)
  mean                 30.50 ns   (29.58 ns .. 33.10 ns)
  std dev              4.628 ns   (1.452 ns .. 9.437 ns)
  variance introduced by outliers: 96% (severely inflated)

  benchmarking multiplication/mul (baseline)
  time                 46.08 ns   (45.71 ns .. 46.43 ns)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 46.12 ns   (45.65 ns .. 46.56 ns)
  std dev              1.564 ns   (1.218 ns .. 2.063 ns)
  variance introduced by outliers: 54% (severely inflated)

  benchmarking division/div
  time                 81.94 ns   (81.11 ns .. 82.84 ns)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 81.97 ns   (81.43 ns .. 82.89 ns)
  std dev              2.422 ns   (1.696 ns .. 3.578 ns)
  variance introduced by outliers: 46% (moderately inflated)

  benchmarking division/div (baseline)
  time                 72.08 ns   (71.26 ns .. 72.89 ns)
                       0.999 R²   (0.999 R² .. 0.999 R²)
  mean                 72.35 ns   (71.52 ns .. 73.15 ns)
  std dev              2.566 ns   (2.203 ns .. 3.044 ns)
  variance introduced by outliers: 55% (severely inflated)

  benchmarking division/mod
  time                 106.6 ns   (105.7 ns .. 107.5 ns)
                       0.999 R²   (0.999 R² .. 0.999 R²)
  mean                 107.9 ns   (106.5 ns .. 109.2 ns)
  std dev              4.520 ns   (3.705 ns .. 6.013 ns)
  variance introduced by outliers: 63% (severely inflated)

  benchmarking division/mod (baseline)
  time                 77.94 ns   (76.87 ns .. 79.02 ns)
                       0.998 R²   (0.998 R² .. 0.999 R²)
  mean                 78.50 ns   (77.49 ns .. 79.74 ns)
  std dev              3.694 ns   (2.729 ns .. 5.235 ns)
  variance introduced by outliers: 68% (severely inflated)
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
