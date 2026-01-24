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

# Run the service
docker run -p 8080:8080 \
  ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt \
  --peer-name <node-host> \
  --peer-port 3001 \
  --network-magic 764824073 \
  --http-port 8080
```

## Quick Start with Nix

```bash
# Setup caching (recommended)
nix shell nixpkgs#cachix -c cachix use paolino

# Run directly
nix run github:paolino/cardano-utxo-csmt -- \
  --peer-name <node-host> \
  --peer-port 3001 \
  --network-magic 764824073 \
  --http-port 8080
```

## Configuration Options

| Option | Environment Variable | Description |
|--------|---------------------|-------------|
| `--peer-name` | `PEER_NAME` | Cardano node hostname |
| `--peer-port` | `PEER_PORT` | Cardano node port (default: 3001) |
| `--network-magic` | `NETWORK_MAGIC` | Network magic number |
| `--http-port` | `HTTP_PORT` | HTTP API port (default: 8080) |
| `--db-path` | `DB_PATH` | RocksDB database path |

### Network Magic Values

| Network | Magic |
|---------|-------|
| Mainnet | 764824073 |
| Preprod | 1 |
| Preview | 2 |

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
