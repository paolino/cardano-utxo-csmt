#!/usr/bin/env bash
# Demo: Mithril Bootstrap with Ed25519 Verification
# Run via record-demo.sh which pre-builds the binary
set -e

echo "=== Cardano UTxO CSMT - Mithril Bootstrap Demo ==="
echo ""

echo "# Step 1: Set up environment variables for Preview"
sleep 1
echo '$ export MITHRIL_AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator'
export MITHRIL_AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator
echo '$ export MITHRIL_ANCILLARY_VERIFICATION_KEY=$(curl -s .../ancillary.vkey)'
export MITHRIL_ANCILLARY_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey)
echo "Verification key loaded (${#MITHRIL_ANCILLARY_VERIFICATION_KEY} chars)"
sleep 2

echo ""
echo "# Step 2: Run cardano-utxo with Mithril bootstrap"
sleep 1
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT
echo '$ cardano-utxo --network preview --mithril-bootstrap --mithril-bootstrap-only --db-path /tmp/db'
echo ""

# Run the pre-built binary - timeout after 60 seconds for demo
timeout 60 result/bin/cardano-utxo \
    --network preview \
    --mithril-bootstrap \
    --mithril-bootstrap-only \
    --db-path "$TMPDIR/db" 2>&1 || true

echo ""
echo "=== Demo Complete ==="
