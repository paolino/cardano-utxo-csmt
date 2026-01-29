#!/usr/bin/env bash
set -euo pipefail

# Mainnet network configuration
# Node exposed on localhost:3039 via Docker
DB_PATH=${DB_PATH:-/tmp/csmt-mainnet}
LOG_PATH=${LOG_PATH:-/tmp/csmt-mainnet.log}
API_PORT=${API_PORT:-8085}
API_DOCS_PORT=${API_DOCS_PORT:-8086}
CLEAN=${CLEAN:-false}

if [[ "$CLEAN" == "true" ]]; then
    echo "Removing existing database: $DB_PATH"
    rm -rf "$DB_PATH"
fi

mkdir -p "$DB_PATH"

echo "Starting cardano-utxo on mainnet network..."
echo "  Node: localhost:3039"
echo "  Database: $DB_PATH"
echo "  Log: $LOG_PATH"
echo "  API: http://localhost:$API_PORT"
echo "  Docs: http://localhost:$API_DOCS_PORT"

exec nix run .#cardano-utxo -- \
    --network mainnet \
    --node-name localhost \
    --port 3039 \
    --csmt-db-path "$DB_PATH" \
    --log-path "$LOG_PATH" \
    --api-port "$API_PORT" \
    --api-docs-port "$API_DOCS_PORT" \
    --enable-metrics-reporting \
    --mithril-bootstrap \
    "$@"
