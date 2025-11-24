# Cardano UTxOs CSMT

Welcome to the documentation for the Cardano UTxOs CSMT project. This documentation provides an overview of the project, its architecture, and how to get started with development and usage.

## What is Cardano UTxOs CSMT?

Cardano UTxOs CSMT is an HTTP service that

- tracks via a chain-follower the UTxO set of a Cardano blockchain
- maintains a Compressed Sparse Merkle Tree (CSMT) representation of the UTxO set
- serves an HTTP API that
    - provides
        - inclusion proofs for UTxOs in the CSMT
        - txouts lookup by txina
    - will provide
        - exclusion proofs for UTxOs not in the CSMT
        - proof of utxos at address completeness

## Installation

### Docker images

Images are available as CI artifacts [here](https://github.com/paolino/cardano-utxo-csmt/actions)

You can build the project using Nix, more instructions are in the plans.

```bash
nix shell nixpkgs#cachix -c cachix use paolino
nix shell github:paolino/cardano-utxo-csmt --refresh
```

## Current status

- [x] Can preload a UTxO set from a cardano-cli dump
- [ ] Can follow the chain
- [ ] Can serve HTTP API

## Demo

```asciinema-player
{
"file": "assets/ingestion.cast"
}
```