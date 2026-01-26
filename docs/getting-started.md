# Getting Started

This guide will help you get the Cardano UTxO CSMT service running.

## Prerequisites

- A running Cardano node with node-to-node protocol access
- Network connectivity to the node's port (default: 3001)

## Quick Start with Docker

The fastest way to get started is using Docker:

```bash
# Download the latest image
gh run download -n cardano-utxo-csmt-image
docker load < output-docker-image

# Run the service (mainnet is default)
docker run -p 8080:8080 \
  ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt \
  --csmt-db-path /data \
  --api-port 8080
```

## Quick Start with Nix

```bash
# Setup caching (recommended)
nix shell nixpkgs#cachix -c cachix use paolino

# Run directly (mainnet is default)
nix run github:paolino/cardano-utxo-csmt -- \
  --csmt-db-path /tmp/csmt-db \
  --api-port 8080
```

## Configuration Options

| Option | Description |
|--------|-------------|
| `--network` | Network: `mainnet`, `preprod`, `preview` (default: mainnet) |
| `--node-name` | Override peer node hostname |
| `--port` | Override peer node port |
| `--csmt-db-path` | RocksDB database path (required) |
| `--api-port` | HTTP API port |
| `--mithril-bootstrap` | Bootstrap from Mithril snapshot |

For preview network with Mithril bootstrap:

```bash
cardano-utxo-chainsync \
  --network preview \
  --mithril-bootstrap \
  --csmt-db-path /tmp/csmt-db
```

## Verifying the Service

Once running, you can check the service status:

```bash
# Check metrics
curl http://localhost:8080/metrics

# View API documentation
open http://localhost:8080/api-docs/swagger-ui
```

## Next Steps

- [API Documentation](swagger-ui.md) - Explore the REST API
- [Architecture](architecture.md) - Understand how it works
