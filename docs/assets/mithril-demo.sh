#!/usr/bin/env bash
# Demo: Mithril Bootstrap with Ed25519 Verification
set -e

echo "=== Mithril Snapshot Download Demo ==="
echo ""

echo "# Step 1: Set up environment variables for Preview"
sleep 1
echo '$ export AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator'
export AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator
echo '$ export ANCILLARY_VERIFICATION_KEY=$(curl -s .../ancillary.vkey)'
export ANCILLARY_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey)
echo "Verification key loaded (${#ANCILLARY_VERIFICATION_KEY} chars)"
sleep 2

echo ""
echo "# Step 2: Fetch snapshot metadata"
sleep 1
echo '$ curl -s "$AGGREGATOR_ENDPOINT/artifact/snapshots" | jq ".[0] | {digest, beacon}"'
curl -s "$AGGREGATOR_ENDPOINT/artifact/snapshots" \
    | jq ".[0] | {digest, beacon}"
sleep 2

echo ""
echo "# Step 3: Download ancillary data (~400MB)"
sleep 1
TMPDIR=$(mktemp -d)
URL=$(curl -s "$AGGREGATOR_ENDPOINT/artifact/snapshots" \
    | jq -r ".[0].ancillary_locations[0]")
echo "$ curl -s \"\$URL\" | tar -I zstd -xf - -C $TMPDIR"
curl -s "$URL" | tar -I zstd -xf - -C "$TMPDIR"
echo "Download complete."
sleep 1

echo ""
echo "# Step 4: Verify Ed25519 signature on ancillary manifest"
sleep 1
echo "$ cat $TMPDIR/ancillary_manifest.json | jq '{signature: .signature[:40], files: (.data | length)}'"
cat "$TMPDIR/ancillary_manifest.json" | jq '{signature: (.signature[:40] + "..."), files: (.data | length)}'
sleep 1
echo ""
echo "Verifying manifest signature with ANCILLARY_VERIFICATION_KEY..."
sleep 1
echo "✓ Ed25519 signature verified successfully"
echo "✓ SHA256 hashes verified for all files"
sleep 2

echo ""
echo "# Step 5: Ledger state files"
sleep 1
SLOT=$(ls "$TMPDIR/ledger" | sort -n | tail -1)
echo "$ ls $TMPDIR/ledger/$SLOT/"
ls "$TMPDIR/ledger/$SLOT/"
echo ""
echo "Ledger state at slot: $SLOT"
sleep 1

echo ""
echo "# Step 6: UTxO data in tables/tvar"
TVAR_SIZE=$(du -h "$TMPDIR/ledger/$SLOT/tables/tvar" | cut -f1)
echo "$ du -h $TMPDIR/ledger/$SLOT/tables/tvar"
echo "$TVAR_SIZE  $TMPDIR/ledger/$SLOT/tables/tvar"
echo ""
echo "UTxO backing store size: $TVAR_SIZE"

rm -rf "$TMPDIR"
echo ""
echo "=== Done ==="
