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

Docker images are available as CI artifacts. Get the latest image from the latest successful run of the `cardano-utxo-csmt-image` workflow.

```bash
gh run download -n cardano-utxo-csmt-image
docker load < output-docker-image
docker run ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt
```

### Nix
You can build the project using Nix, more instructions are in the plans.

Setup caching with Cachix:

```bash
nix shell nixpkgs#cachix -c cachix use paolino
```

To get a shell with the project and its dependencies, run:

```bash
nix shell github:paolino/cardano-utxo-csmt
```

To build the docker image, run:


```bash
nix build github:paolino/cardano-utxo-csmt#docker-image
docker load < result
version=$(nix eval --raw github:paolino/cardano-utxo-csmt#version)
docker run "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt:$version"

```

To build an arx for linux, run:

```bash
nix bundle github:paolino/cardano-utxo-csmt -o utxo-csmt
./utxo-csmt
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