#!/usr/bin/env bash
# Script to record the mithril bootstrap demo
# Ensures nix is built before recording to avoid build output in demo
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
CAST_FILE="$SCRIPT_DIR/mithril-bootstrap.cast"
DEMO_SCRIPT="$SCRIPT_DIR/mithril-demo.sh"

cd "$PROJECT_ROOT"

echo "=== Pre-building with nix ==="
nix build --quiet .#cardano-utxo-chainsync
echo "Build complete."
echo ""

echo "=== Recording demo ==="
asciinema rec --overwrite -c "$DEMO_SCRIPT" "$CAST_FILE"

echo ""
echo "Recording saved to: $CAST_FILE"
