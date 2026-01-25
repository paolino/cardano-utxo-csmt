#!/usr/bin/env bash
# Demo: Mithril Bootstrap for Cardano UTxO CSMT
# This script shows how to use Mithril to quickly bootstrap the database

set -e

echo "=== Mithril Bootstrap Demo ==="
echo ""
echo "# Step 1: Check available Mithril options"
sleep 2
echo ""
echo '$ cardano-utxo-chainsync --help | grep -A2 mithril'
cardano-utxo-chainsync --help 2>/dev/null | grep -A2 mithril || true
sleep 3

echo ""
echo "# Step 2: Fetch latest snapshot metadata from preview network"
sleep 2
echo ""
echo '$ curl -s "https://aggregator.pre-release-preview.api.mithril.network/aggregator/artifact/snapshots" | jq ".[0]"'
curl -s "https://aggregator.pre-release-preview.api.mithril.network/aggregator/artifact/snapshots" | jq ".[0]"
sleep 3

echo ""
echo "# Step 3: Bootstrap with Mithril (preview network example)"
sleep 2
echo ""
echo '$ cardano-utxo-chainsync \'
echo '    --mithril-bootstrap \'
echo '    --mithril-network preview \'
echo '    --csmt-db-path /tmp/csmt-db \'
echo '    --node-name preview-node.world.dev.cardano.org \'
echo '    --port 30002 \'
echo '    --network-magic 2'
echo ""
echo "# This will:"
echo "#   1. Download the latest certified snapshot (~400MB ancillary data)"
echo "#   2. Extract ledger state to temporary directory"
echo "#   3. Import UTxOs into the CSMT database"
echo "#   4. Continue chain sync from the snapshot point"
sleep 3

echo ""
echo "=== Demo Complete ==="
