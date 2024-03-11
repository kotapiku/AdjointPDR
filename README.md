# AdjointPDR

An implementation of [AdjointPDR](https://arxiv.org/abs/2307.02817).

## How to use

Before running, you need to install z3.

- For benchmarks, please run `cabal bench`. This will execute `app/bench/BenchmarkMDP.hs`.
- The core of AdjointPDR is the function `adjointPDR` in `src/AdjointPDR.hs`. It is a template for AdjointPDR, and you can obtain an instance by specifying two complete lattices L and C (`CLatPN p n`), heuristics (`Heuristics p n`), and a problem `mu b <=^? p in L` you wish to solve (`Problem p`).

## Requirements

- ghc 9.4.8
- cabal 3.10.2

## Docker

We provide a Dockerfile to simplify the management of dependecies.
To build the image, just run
```sh
docker build -t adjoint-pdr .
```
Then, to execute the benchmarks, just run a container from that image:
```sh
docker run -it adjoint-pdr
```
To open an interactive shell inside the container, run
```sh
docker run -it adjoint-pdr /bin/sh
```
