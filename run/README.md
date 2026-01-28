# Local Run Scripts

Scripts to run cardano-utxo against local Cardano nodes running in Docker.

## Prerequisites

Local nodes must be running and exposing their ports:

| Network | Docker Container | Port |
|---------|-----------------|------|
| Preview | cardano-node-preview | 3037 |
| Preprod | cardano-node-preprod | 3038 |
| Mainnet | mainnet-cardano-node-mainnet-1 | 3039 |

## Usage

```bash
# Run against preview network
./run/preview.sh

# Run against preprod network
./run/preprod.sh

# Run against mainnet network
./run/mainnet.sh
```

## Configuration

Environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `DB_PATH` | RocksDB database path | `/tmp/csmt-{network}` |
| `API_PORT` | HTTP API port | 8081/8083/8085 |
| `API_DOCS_PORT` | Swagger docs port | 8082/8084/8086 |

Example:
```bash
DB_PATH=/data/csmt-preview API_PORT=9000 ./run/preview.sh
```

## Additional Options

Pass extra flags after the script:

```bash
# Enable Mithril bootstrap
./run/preprod.sh --mithril-bootstrap

# Start from specific slot
./run/preview.sh --starting-point 12345678
```

## Endpoints

Once running:

- **Metrics**: `http://localhost:{API_PORT}/metrics`
- **Ready**: `http://localhost:{API_PORT}/ready`
- **Swagger**: `http://localhost:{API_DOCS_PORT}/`
