
# Amplitude Estimation & Grover (HQP / Haskell)

## Overview
This project implements **Grover’s algorithm (amplitude amplification)** and **Quantum Amplitude Estimation (AE)** in **Haskell** using the **HQP framework**. We extended HQP with our own program module and several executable test drivers.

We initially used HQP’s *matrix semantics*, but switched to the *state vector semantics* after an HQP update, because it applies quantum operations lazily to a state vector and allowed us to run experiments with a higher number of qubits. The only change we made to the framework code was adjusting the simulator tolerance from `1e-12` to `1e-10`, since AE circuits become deep (controlled powers `Q^(2^j)` + inverse QFT) and floating-point round-off noise started causing issues at higher qubit counts.

## Project structure
We used the HQP framework and extended it with:

- `src/HQP/Programs/Grovers.hs`  
  Contains our **Grover** and **Amplitude Estimation** implementations, plus helper functions.

Executables (tests/benchmarks):

- `exe/TestGrover.hs` — Grover test
- `exe/TestAE.hs` — initial AE test
- `exe/Test2AE.hs` — AE benchmark sweep over multiple seeds, `n`, and `m` (outputs CSV)
- `exe/TestIntegration.hs` — full pipeline test: AE -> estimate optimal Grover rounds -> run Grover (supports multiple marked solutions)

## Requirements / Dependencies

### Haskell / HQP
- **GHC** (Haskell compiler)
- **Cabal** (build tool)
- HQP project dependencies as specified in the `.cabal` file

> Note: If you see build errors related to BLAS/LAPACK (e.g., through `hmatrix`), you may need to install system BLAS/LAPACK development libraries. The package names depend on your OS/distribution.

### Python analysis (plots + CSV processing)
We created a **Jupyter Notebook** (`analysis.ipynb`) to run our Python analysis script for the CSV output (plots for runtime/error vs `m`, etc.).


- **Python 3.13**
- Libraries:
  - `numpy`
  - `pandas`
  - `matplotlib`

## Build instructions (Haskell)
From the project root:

```bash
cabal update
cabal build
````

To build a specific executable:

```bash
cabal build TestGrover
cabal build TestAE
cabal build Test2AE
cabal build TestIntegration
```

## Run instructions (Haskell)

Run executables using:

```bash
cabal run TestGrover
cabal run TestAE
cabal run Test2AE
cabal run TestIntegration
```

## Running tests: Cabal vs GHCi
We initially ran the tests inside **GHCi**, but we found they were noticeably faster when compiled and executed via **Cabal**. Because of this, we updated the `hqp.cabal` file to expose our `HQP.Programs.Grovers` module and to include the test executables.

### Recommended (faster): build + run with Cabal
```bash
cabal build TestGrover
cabal run TestGrover
````

### Interactive (slower): run from GHCi

```bash
cabal repl
```

Then inside GHCi you can load a test file, for example:

```text
:l exe/TestGrover.hs
```

You can do the same for the other tests:

* `:l exe/TestAE.hs`
* `:l exe/Test2AE.hs`
* `:l exe/TestIntegration.hs`


### AE benchmark output (CSV)

This is how we generated the results CSV used for plots:

```bash
cabal build Test2AE
cabal run Test2AE -- > results.csv
```

The CSV includes columns such as: seed, `n`, `m`, marked solution, measured `y`/`phi`, estimated `aHat`, absolute error, true `a`, and CPU time.

## Notes

* Runtime grows very quickly with qubit count because the simulator state size is `2^(m+n)` and AE uses controlled powers up to `Q^(2^(m-1))`.
* The simulator tolerance was relaxed from `1e-12` to `1e-10` to avoid instability from accumulated floating-point noise in deeper AE circuits.

