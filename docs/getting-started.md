# Getting Started

This guide will help you get the Cardano UTxO CSMT service running on preprod.

## Prerequisites

- A running Cardano node with node-to-node protocol access
- Network connectivity to the node's port (default: 3001)

## Setup Environment Variables

Mithril bootstrap requires environment variables for verification. Download and export them for your network:

=== "Preprod"

    ```bash
    export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
    export GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)
    export ANCILLARY_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey)
    ```

=== "Preview"

    ```bash
    export AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator
    export GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey)
    export ANCILLARY_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey)
    ```

=== "Mainnet"

    ```bash
    export AGGREGATOR_ENDPOINT=https://aggregator.release-mainnet.api.mithril.network/aggregator
    export GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey)
    export ANCILLARY_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey)
    ```

Source: [Mithril Network Configurations](https://mithril.network/doc/manual/getting-started/network-configurations)

## Quick Start

> **Coming Soon:** Docker quickstart with pre-configured environment.

### Nix (Preprod)

```bash
# Setup caching (recommended)
nix shell nixpkgs#cachix -c cachix use paolino

# Run on preprod with Mithril bootstrap
nix run github:paolino/cardano-utxo-csmt -- \
  --network preprod \
  --mithril-bootstrap \
  --csmt-db-path /tmp/csmt-db \
  --api-port 8080
```

This will:

1. Download the latest Mithril snapshot for preprod
2. Verify the Ed25519 signature on the ancillary manifest
3. Extract and import the UTxO set into the CSMT database
4. Start chain sync from the snapshot slot

## Configuration Options

| Option | Description |
|--------|-------------|
| `--network` | Network: `mainnet`, `preprod`, `preview` (default: mainnet) |
| `--node-name` | Override peer node hostname |
| `--port` | Override peer node port |
| `--csmt-db-path` | RocksDB database path (required) |
| `--api-port` | HTTP API port for REST endpoints |
| `--api-docs-port` | HTTP port for Swagger UI documentation |
| `--mithril-bootstrap` | Bootstrap from Mithril snapshot |
| `--ancillary-verification-key` | Ed25519 key for ancillary verification |
| `--genesis-verification-key` | Genesis key for mithril-client CLI |
| `--aggregator-endpoint` | Mithril aggregator URL |
| `--mithril-skip-ancillary-verification` | Skip Ed25519 verification (not recommended) |

## Environment Variables

| Variable | Description |
|----------|-------------|
| `AGGREGATOR_ENDPOINT` | Mithril aggregator URL |
| `GENESIS_VERIFICATION_KEY` | Genesis verification key (JSON-hex) |
| `ANCILLARY_VERIFICATION_KEY` | Ed25519 ancillary verification key (JSON-hex) |

## Verifying the Service

Once running, check the service status:

```bash
# Check readiness
curl http://localhost:8080/ready

# Check metrics
curl http://localhost:8080/metrics

# View API documentation
open http://localhost:8080/api-docs/swagger-ui
```

## Next Steps

- [Mithril Bootstrap](mithril-bootstrap.md) - Details on Mithril bootstrapping
- [API Documentation](swagger-ui.md) - Explore the REST API
- [Architecture](architecture.md) - Understand how it works
