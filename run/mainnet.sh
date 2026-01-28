#!/usr/bin/env bash
set -euo pipefail

# Mainnet network configuration
# Node exposed on localhost:3039 via Docker
DB_PATH=${DB_PATH:-/tmp/csmt-mainnet}
API_PORT=${API_PORT:-8085}
API_DOCS_PORT=${API_DOCS_PORT:-8086}

mkdir -p "$DB_PATH"

echo "Starting cardano-utxo on mainnet network..."
echo "  Node: localhost:3039"
echo "  Database: $DB_PATH"
echo "  API: http://localhost:$API_PORT"
echo "  Docs: http://localhost:$API_DOCS_PORT"

exec cabal run cardano-utxo -O0 -- \
    --network mainnet \
    --node-name localhost \
    --port 3039 \
    --csmt-db-path "$DB_PATH" \
    --api-port "$API_PORT" \
    --api-docs-port "$API_DOCS_PORT" \
    --enable-metrics-reporting \
    "$@"
