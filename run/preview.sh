#!/usr/bin/env bash
set -euo pipefail

# Preview network configuration
# Node exposed on localhost:3037 via Docker
DB_PATH=${DB_PATH:-/tmp/csmt-preview}
LOG_PATH=${LOG_PATH:-/tmp/csmt-preview.log}
API_PORT=${API_PORT:-8081}
API_DOCS_PORT=${API_DOCS_PORT:-8082}
CLEAN=${CLEAN:-false}

if [[ "$CLEAN" == "true" ]]; then
    echo "Removing existing database: $DB_PATH"
    rm -rf "$DB_PATH"
fi

mkdir -p "$DB_PATH"

echo "Starting cardano-utxo on preview network..."
echo "  Node: localhost:3037"
echo "  Database: $DB_PATH"
echo "  Log: $LOG_PATH"
echo "  API: http://localhost:$API_PORT"
echo "  Docs: http://localhost:$API_DOCS_PORT"

exec cabal run cardano-utxo -- \
    --network preview \
    --node-name localhost \
    --port 3037 \
    --csmt-db-path "$DB_PATH" \
    --log-path "$LOG_PATH" \
    --api-port "$API_PORT" \
    --api-docs-port "$API_DOCS_PORT" \
    --mithril-bootstrap \
    "$@"
