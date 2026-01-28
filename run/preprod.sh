#!/usr/bin/env bash
set -euo pipefail

# Preprod network configuration
# Node exposed on localhost:3038 via Docker
DB_PATH=${DB_PATH:-/tmp/csmt-preprod}
API_PORT=${API_PORT:-8083}
API_DOCS_PORT=${API_DOCS_PORT:-8084}

mkdir -p "$DB_PATH"

echo "Starting cardano-utxo on preprod network..."
echo "  Node: localhost:3038"
echo "  Database: $DB_PATH"
echo "  API: http://localhost:$API_PORT"
echo "  Docs: http://localhost:$API_DOCS_PORT"

exec cabal run cardano-utxo -O0 -- \
    --network preprod \
    --node-name localhost \
    --port 3038 \
    --csmt-db-path "$DB_PATH" \
    --api-port "$API_PORT" \
    --api-docs-port "$API_DOCS_PORT" \
    --enable-metrics-reporting \
    "$@"
