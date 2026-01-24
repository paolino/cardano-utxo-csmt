# Cardano UTxO CSMT

A service that maintains a Compact Sparse Merkle Tree over Cardano's UTxO set, enabling efficient inclusion proofs.

## Features

- **Real-time Sync**: Follows the Cardano blockchain via node-to-node protocol
- **Merkle Proofs**: Generate cryptographic inclusion proofs for any UTxO
- **Multi-Era Support**: Works with all Cardano eras (Byron through Conway)
- **Rollback Handling**: Gracefully handles chain reorganizations
- **REST API**: Simple HTTP interface for queries and proofs

## How It Works

The service connects to a Cardano node and maintains:

1. **UTxO Set**: Current unspent transaction outputs
2. **CSMT**: A Merkle tree with path compression for efficient proofs
3. **Rollback History**: State snapshots for handling chain reorgs

Each block updates the tree, producing a new Merkle root that commits to the entire UTxO set state.

## Use Cases

- **Light Clients**: Verify UTxO existence without downloading the full chain
- **Bridges**: Prove UTxO state to external systems
- **Auditing**: Cryptographic proof of UTxO set at any point

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

See [Getting Started](getting-started.md) for detailed setup instructions.

## Installation

### Docker

```bash
gh run download -n cardano-utxo-csmt-image
docker load < output-docker-image
```

### Nix

```bash
# Setup caching
nix shell nixpkgs#cachix -c cachix use paolino

# Run directly
nix run github:paolino/cardano-utxo-csmt

# Or build a standalone binary
nix bundle github:paolino/cardano-utxo-csmt -o utxo-csmt
```

## Project Status

- [x] Chain synchronization via node-to-node protocol
- [x] UTxO set tracking with RocksDB persistence
- [x] CSMT maintenance with Merkle root computation
- [x] HTTP API with Swagger documentation
- [x] Merkle roots endpoint
- [x] Inclusion proofs endpoint
- [ ] Exclusion proofs
- [ ] Address completeness proofs

## Links

- [GitHub Repository](https://github.com/paolino/cardano-utxo-csmt)
- [API Documentation](swagger-ui.md)
- [Architecture Overview](architecture.md)
