# AdjointPDR

An implementation of [AdjointPDR](https://arxiv.org/abs/2307.02817).

## How to use

Before running, you need to install z3.

- For benchmarks, please run `cabal bench`. This will execute `app/bench/BenchmarkMDP.hs`.
- The core of AdjointPDR^AI is the function `adjointPDR` in `src/AdjointPDR.hs`. You can obtain an instance of AdjointPDR^AI by specifying two complete lattices L and C (`CLatPN p n`), heuristics (`Heuristics p n`), and the problem you wish to solve (`Problem p`).

## Requirements

- ghc 9.4.8
- cabal 3.10.2
