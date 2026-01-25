#!/usr/bin/env bash
# Demo: Mithril Bootstrap - Working Functionality
set -e

echo "=== Mithril Snapshot Download Demo ==="
echo ""

echo "# Step 1: Fetch snapshot metadata"
sleep 1
echo '$ curl -s ".../artifact/snapshots" | jq ".[0] | {digest, beacon, ancillary_size}"'
curl -s "https://aggregator.pre-release-preview.api.mithril.network/aggregator/artifact/snapshots" \
    | jq ".[0] | {digest, beacon, ancillary_size}"
sleep 2

echo ""
echo "# Step 2: Download ledger state (~400MB)"
sleep 1
TMPDIR=$(mktemp -d)
URL=$(curl -s "https://aggregator.pre-release-preview.api.mithril.network/aggregator/artifact/snapshots" \
    | jq -r ".[0].ancillary_locations[0]")
echo "$ curl -s \"\$URL\" | tar -I zstd -xf - -C $TMPDIR"
curl -s "$URL" | tar -I zstd -xf - -C "$TMPDIR"
echo "Done."
sleep 1

echo ""
echo "# Step 3: Ledger state files"
sleep 1
SLOT=$(ls "$TMPDIR/ledger" | sort -n | tail -1)
echo "$ ls $TMPDIR/ledger/$SLOT/"
ls "$TMPDIR/ledger/$SLOT/"
echo ""
echo "Ledger state at slot: $SLOT"

rm -rf "$TMPDIR"
echo ""
echo "=== Done ==="
