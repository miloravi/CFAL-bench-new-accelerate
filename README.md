# CFAL benchmarks in the new version of Accelerate

This repository contains implementations of the [CFAL benchmarks](https://github.com/diku-dk/CFAL-bench) in the new, currently unreleased, version of Accelerate. Benchmarks nbody-naive and MG are adapted to the new version of Accelerate.

To run the benchmarks, first install Haskell and Stack via [GHCup](https://www.haskell.org/ghcup/), LLVM 15 (for instance via the [automatic installation script](https://apt.llvm.org)) and coinor-cbc. Within the project folder of the benchmark (`nbody-naive/accelerate/nbody-naive` or `MG/accelerate`), run `stack run` to run the benchmarks on all cores and `ACCELERATE_LLVM_NATIVE_THREADS=1 stack run` to run them on a single core.

A version of quickhull is available at [ivogabe/quickhull-benchmarks](https://github.com/ivogabe/quickhull-benchmarks), where we also experimented with more variants of Quickhull. In contrast to the original implementation for CFAL, which is entirely flattened, we also have partially flattened variants.
