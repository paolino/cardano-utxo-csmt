# CSMT

[![CI](https://github.com/paolino/cardano-utxo-csmt/actions/workflows/CI.yaml/badge.svg)](https://github.com/paolino/cardano-utxo-csmt/actions/workflows/CI.yaml)

## Installation

Currently there is no packaging available. You can clone the repository and build the project using [Cabal](https://www.haskell.org/cabal/):

```bash
git clone https://github.com/paolino/cardano-utxo-csmt.git
cd cardano-utxo-csmt
cabal install
```
This will install the library, CLI tool.

Nix support is also available. You can build the project using Nix:

```bash
nix shell github:paolino/cardano-utxo-csmt --refresh
```

will drop you in a shell where cardano-utxo-csmt is built and available.
