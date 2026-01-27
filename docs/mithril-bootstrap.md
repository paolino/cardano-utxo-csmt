# Mithril Bootstrap

Mithril enables fast bootstrapping of the Cardano UTxO CSMT database by downloading
certified snapshots instead of syncing from genesis.

## Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Fetch snapshot metadata | ✅ Working | All networks (mainnet, preprod, preview) |
| HTTP download | ✅ Working | Downloads ancillary data with ledger state |
| Ed25519 ancillary verification | ✅ Working | Verifies manifest signature for all networks |
| Find ledger state files | ✅ Working | Supports both old (.lstate) and new UTxO-HD formats |
| CLI options | ✅ Working | `--network`, `--mithril-bootstrap`, etc. |
| UTxO extraction from ledger state | ✅ Working | Streams MemPack-encoded (TxIn, TxOut) pairs from tvar file |
| TxIn/TxOut decoding | ✅ Verified | Decodes as Conway-era `TxIn` and `BabbageTxOut` |
| CBOR re-encoding | ✅ Working | Converts MemPack to CBOR for chain sync compatibility |
| STM certificate verification | 🔮 Planned | See [FFI plan](mithril-stm-ffi-plan.md) on `feature/mithril-stm-ffi-plan` branch |

## Overview

Mithril bootstrapping:

1. Downloads the latest certified snapshot from Mithril aggregators
2. Extracts the UTxO set from the InMemory backing store (`tables/tvar`)
3. Decodes MemPack to Conway-era types, re-encodes to CBOR for chain sync compatibility
4. Streams into CSMT database using the standard import interface

This provides a fast way to obtain UTxO set data without syncing from genesis.

## What Is a Mithril Snapshot?

A Mithril snapshot captures the **ledger state** at a slot deep in the immutable
history of the Cardano blockchain.

**Key distinction:**

- The snapshot is **not** the immutable DB (blocks) itself
- It is the **ledger state** (UTxO set, stake distribution, etc.) that results
  from processing those blocks
- The slot is past the security parameter (k=2160 blocks), making it final

This is what makes Mithril useful: you get the final state without replaying
all the blocks from genesis.

**Trust model:**

The ledger state is cryptographically signed by Cardano stake pool operators
using Mithril's STM (Stake-based Threshold Multi-signatures). Verifying the
certificate chain proves the snapshot was produced correctly by the network.

## Demo

```asciinema-player
{ "file": "assets/mithril-bootstrap.cast"
, "mkap_theme": "none"
, "cols": 100
}
```

## Usage

Enable Mithril bootstrap with the `--mithril-bootstrap` flag:

```bash
cardano-utxo-chainsync \
    --network preview \
    --mithril-bootstrap \
    --csmt-db-path /path/to/db
```

The `--network` option sets both the Cardano network (magic, default peer) and
the Mithril network automatically.

## CLI Options

| Option | Description | Default |
|--------|-------------|---------|
| `--network NETWORK` | Network: `mainnet`, `preprod`, `preview` | `mainnet` |
| `--mithril-bootstrap` | Enable Mithril bootstrapping | `false` |
| `--mithril-bootstrap-only` | Exit after bootstrap (skip chain sync) | `false` |
| `--mithril-aggregator URL` | Override aggregator URL | Network default |
| `--mithril-download-dir DIR` | Directory for downloads | Temp directory |
| `--mithril-ancillary-verification-key KEY` | Override Ed25519 verification key (JSON-hex) | Network default |
| `--mithril-skip-ancillary-verification` | Skip Ed25519 verification (not recommended) | `false` |

## Networks

| Network | Aggregator URL | Ancillary Verification |
|---------|---------------|------------------------|
| Mainnet | `https://aggregator.release-mainnet.api.mithril.network/aggregator` | ✅ Ed25519 |
| Preprod | `https://aggregator.release-preprod.api.mithril.network/aggregator` | ✅ Ed25519 |
| Preview | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` | ✅ Ed25519 |

Ancillary verification keys are sourced from the official Mithril infrastructure:
[`mithril-infra/configuration/{network}/ancillary.vkey`](https://mithril.network/doc/manual/getting-started/network-configurations)

!!! note "Shared Keys"
    Preview and Preprod currently share the same verification key.
    Mainnet has a distinct key.

## How It Works

### 1. Fetch Snapshot Metadata

The client queries the Mithril aggregator API for the latest snapshot:

```bash
curl -s "https://aggregator.pre-release-preview.api.mithril.network/aggregator/artifact/snapshots" | jq ".[0]"
```

Response includes:
- `digest`: Unique snapshot identifier
- `beacon`: Slot and epoch information
- `locations`: Download URLs for immutable files
- `ancillary_locations`: Download URLs for ledger state

### 2. Download and Verify

**HTTP Download with Ed25519 Verification**

- Downloads ancillary archive directly from CDN URLs
- Verifies `ancillary_manifest.json` Ed25519 signature
- Verifies SHA256 hash of each file against manifest
- Fast, no external dependencies

The ancillary archive contains a signed manifest (`ancillary_manifest.json`) that lists
all files with their SHA256 hashes, plus an Ed25519 signature over the manifest data.
Verification ensures:

1. The manifest signature is valid (signed by IOG's ancillary key)
2. Each extracted file matches its declared hash

!!! note "STM Certificate Chain"
    Ed25519 verification protects the ancillary files (ledger state).
    Full STM certificate chain verification is still planned for the immutable files.
    See [STM verification plan](mithril-stm-ffi-plan.md) for details.

### 3. Ledger State Format

The snapshot contains ledger state in the UTxO-HD format:

```
ledger/
├── 102673454/
│   ├── meta
│   ├── state
│   └── tables/
│       └── tvar
└── 102672637/
    ├── meta
    ├── state
    └── tables/
        └── tvar
```

Each directory is named by slot number and contains the serialized ledger state.

### 4. UTxO Extraction

The UTxO set is stored in the `tables/tvar` file within the ledger state directory.
This file uses the InMemory backing store format:

```
tvar file format:
├── List[1] - wrapper
└── Map (indefinite-length CBOR)
    ├── (bytes: TxIn, bytes: TxOut)
    ├── (bytes: TxIn, bytes: TxOut)
    └── ...
```

Each key-value pair contains MemPack-encoded `TxIn` and `TxOut` (stored as CBOR bytes in the map).
The extraction streams these pairs without full ledger state decoding, making it
memory-efficient for large UTxO sets (preview: ~560MB, mainnet: much larger).

### 5. Stream Processing

The extraction yields a stream of `(ByteString, ByteString)` pairs:

- Key: MemPack-encoded `TxIn` (decodable via `Data.MemPack.unpack`)
- Value: MemPack-encoded `TxOut` (decodable via `Data.MemPack.unpack`)

The E2E test verifies that extracted bytes decode successfully as
Conway-era `Cardano.Ledger.TxIn.TxIn` and `Cardano.Ledger.Babbage.TxOut.BabbageTxOut`.

Progress is reported every 10,000 entries during extraction.

### 6. Chain Sync Continuation

!!! warning "Known Limitation"
    The current implementation replays from genesis after Mithril import.
    This negates most of the bootstrap speed benefit. See issue #32 for the fix.

After importing the UTxO set, chain sync needs to continue from where the
snapshot left off. This requires a `Point(slot, block_hash)`.

**The Problem:**

- Mithril provides the **slot number** (from ledger state directory name)
- Mithril does **not** provide the **block hash**
- Without the hash, we cannot construct a valid `Point`

**Current Behavior:**

The base checkpoint is set to `Origin`, causing chain sync to replay from
genesis. This works correctly (CSMT operations are idempotent) but is slow.

**Planned Fix (Issue #32):**

After Mithril import, perform a header-only chain sync scan:

1. Connect to node, find intersection at Origin
2. Scan headers until reaching the Mithril slot
3. Extract block hash from that header
4. Save `Point(slot, hash)` as checkpoint
5. Disconnect and reconnect with proper checkpoint

This adds a few seconds of header scanning but avoids replaying all blocks.

## Security Considerations

### Ed25519 Ancillary Verification (Implemented)

Ancillary files (ledger state) are protected by Ed25519 signature verification:

- **Manifest signature**: The `ancillary_manifest.json` is signed by IOG's Ed25519 key
- **File integrity**: Each file's SHA256 hash is verified against the manifest
- **Network keys**: Official verification keys are embedded for all networks

This verification runs automatically. To skip it (not recommended):

```bash
cardano-utxo-chainsync \
    --mithril-bootstrap \
    --mithril-skip-ancillary-verification \
    ...
```

To use a custom verification key:

```bash
cardano-utxo-chainsync \
    --mithril-bootstrap \
    --mithril-ancillary-verification-key "5b32332c37312c..." \
    ...
```

### STM Certificate Chain (Planned)

Full STM certificate chain verification for immutable files is planned.
This will verify that snapshots are correctly signed by Cardano stake pool operators.

See the [Mithril STM FFI plan](mithril-stm-ffi-plan.md) for the implementation roadmap.

## Troubleshooting

### Snapshot Download Fails

Check network connectivity to the aggregator:
```bash
curl -I "https://aggregator.pre-release-preview.api.mithril.network/aggregator/artifact/snapshots"
```

### Ledger State Not Found

Ensure `--include-ancillary` is used (automatic in this implementation).
The ledger state is in the ancillary data, not the main snapshot.

### Large Download Size

Preview network ancillary data is ~400MB (tvar file ~560MB after extraction).
Mainnet is significantly larger.
Ensure sufficient disk space and bandwidth.

### Extraction Errors

If extraction fails with "TablesNotFound":
- Verify the ledger state directory contains `tables/tvar`
- Ensure the download completed successfully

If extraction fails with "TablesDecodeFailed":
- The tvar file format may have changed
- Check for updates to ouroboros-consensus
