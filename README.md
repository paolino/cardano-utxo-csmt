# Cardano UTxO CSMT

[![CI](https://github.com/paolino/cardano-utxo-csmt/actions/workflows/CI.yaml/badge.svg)](https://github.com/paolino/cardano-utxo-csmt/actions/workflows/CI.yaml)
[![Documentation](https://github.com/paolino/cardano-utxo-csmt/actions/workflows/deploy-docs.yaml/badge.svg)](https://github.com/paolino/cardano-utxo-csmt/actions/workflows/deploy-docs.yaml)

An HTTP service that maintains a Compact Sparse Merkle Tree (CSMT) over Cardano's UTxO set, enabling efficient cryptographic inclusion proofs.

## Features

- **Real-time Chain Sync**: Follows the blockchain via node-to-node protocol
- **Merkle Proofs**: Generate inclusion proofs for any UTxO
- **Multi-Era Support**: Byron through Conway
- **Rollback Handling**: Graceful chain reorganization support
- **REST API**: Simple HTTP interface with Swagger documentation

## Quick Start

```bash
# Using Docker
docker run -p 8080:8080 \
  ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt \
  --peer-name <node-host> \
  --peer-port 3001 \
  --network-magic 764824073

# Check status
curl http://localhost:8080/metrics
```

## Installation

### Docker

```bash
gh run download -n cardano-utxo-csmt-image
docker load < output-docker-image
```

### Nix

```bash
nix shell nixpkgs#cachix -c cachix use paolino
nix run github:paolino/cardano-utxo-csmt
```

## Documentation

Full documentation available at **[paolino.github.io/cardano-utxo-csmt](https://paolino.github.io/cardano-utxo-csmt/)**

- [Getting Started](https://paolino.github.io/cardano-utxo-csmt/getting-started/)
- [Architecture](https://paolino.github.io/cardano-utxo-csmt/architecture/)
- [API Reference](https://paolino.github.io/cardano-utxo-csmt/swagger-ui/)

## License

Apache-2.0
