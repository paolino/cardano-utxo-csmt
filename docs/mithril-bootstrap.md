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

## Bootstrap Time Estimates

Bootstrap time depends on three phases:

| Phase | Description |
|-------|-------------|
| **Download** | Download snapshot from Mithril CDN (~400MB for preview) |
| **Extraction** | Extract and import UTxOs into CSMT database |
| **Header Sync** | Sync headers from genesis to Mithril slot (skip mode) |

### Estimated Times by Network

| Network | UTxOs | Slots | Download | Extraction | Header Sync | **Total** |
|---------|-------|-------|----------|------------|-------------|-----------|
| Preview | ~2.9M | ~103M | ~5 min | ~1 hour | ~1.5 hours | **~2.5 hours** |
| Preprod | ~5M | ~75M | ~10 min | ~2 hours | ~1 hour | **~3.5 hours** |
| Mainnet | ~20M | ~140M | ~30 min | ~8 hours | ~2 hours | **~10-12 hours** |

**Notes:**

- Times measured with a **local cardano-node** (same machine or LAN)
- Hardware: SSD storage, 100Mbps+ connection for Mithril download
- Extraction rate: ~800-1000 UTxOs/second
- Header sync rate: ~15,000-20,000 slots/second (depends on node proximity)
- Mainnet estimates are projections based on preview/preprod measurements
- Remote nodes will have slower header sync due to network latency

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

## Usage

Enable Mithril bootstrap with the `--mithril-bootstrap` flag:

```bash
cardano-utxo \
    --network preview \
    --mithril-bootstrap \
    --db-path /path/to/db
```

The `--network` option sets both the Cardano network (magic, default peer) and
the Mithril network automatically.

## Environment Variables

Set up environment variables for your network before running:

=== "Preprod"

    ```bash
    export MITHRIL_AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
    export MITHRIL_GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)
    export MITHRIL_ANCILLARY_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey)
    ```

=== "Preview"

    ```bash
    export MITHRIL_AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator
    export MITHRIL_GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey)
    export MITHRIL_ANCILLARY_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey)
    ```

=== "Mainnet"

    ```bash
    export MITHRIL_AGGREGATOR_ENDPOINT=https://aggregator.release-mainnet.api.mithril.network/aggregator
    export MITHRIL_GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey)
    export MITHRIL_ANCILLARY_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey)
    ```

Source: [Mithril Network Configurations](https://mithril.network/doc/manual/getting-started/network-configurations)

> **Note:** Preview and Preprod currently share the same ancillary verification key. Mainnet has a distinct key.

## CLI Options

| Option | Description | Default |
|--------|-------------|---------|
| `--network NETWORK` | Network: `mainnet`, `preprod`, `preview` | `mainnet` |
| `--mithril-bootstrap` | Enable Mithril bootstrapping | `false` |
| `--mithril-bootstrap-only` | Exit after bootstrap (skip chain sync) | `false` |
| `--mithril-aggregator-endpoint URL` | Override aggregator URL | From env var |
| `--mithril-genesis-verification-key KEY` | Genesis verification key (JSON-hex) | From env var |
| `--mithril-ancillary-verification-key KEY` | Ancillary verification key (JSON-hex) | From env var |
| `--mithril-download-dir DIR` | Directory for downloads | Temp directory |
| `--mithril-skip-ancillary-verification` | Skip Ed25519 verification (not recommended) | `false` |

**Environment Variables:**

| Variable | Description |
|----------|-------------|
| `MITHRIL_AGGREGATOR_ENDPOINT` | Mithril aggregator URL |
| `MITHRIL_GENESIS_VERIFICATION_KEY` | Genesis verification key for mithril-client CLI (JSON-hex) |
| `MITHRIL_ANCILLARY_VERIFICATION_KEY` | Ed25519 verification key for ancillary files (JSON-hex) |

## Networks

| Network | Aggregator URL | Ancillary Verification |
|---------|---------------|------------------------|
| Mainnet | `https://aggregator.release-mainnet.api.mithril.network/aggregator` | ✅ Ed25519 |
| Preprod | `https://aggregator.release-preprod.api.mithril.network/aggregator` | ✅ Ed25519 |
| Preview | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` | ✅ Ed25519 |

## Bootstrap Design

This section describes the complete bootstrap flow and the design decisions behind it.

### Bootstrap Phases

The bootstrap process consists of five phases, tracked via the `/metrics` endpoint:

```
┌─────────────┐    ┌──────────┐    ┌────────────┐    ┌─────────────────┐    ┌────────┐
│ Downloading │───▶│ Counting │───▶│ Extracting │───▶│ Syncing Headers │───▶│ Synced │
└─────────────┘    └──────────┘    └────────────┘    └─────────────────┘    └────────┘
```

| Phase | Description | Progress Tracking |
|-------|-------------|-------------------|
| `downloading` | Fetching Mithril snapshot from CDN | `downloadedBytes` |
| `counting` | Counting UTxOs in snapshot | `countingProgress` |
| `extracting` | Importing UTxOs into CSMT | `extractionProgress` (current, total, rate, eta) |
| `syncing_headers` | Syncing headers to reach Mithril slot | `headerSyncProgress` (currentSlot, targetSlot) |
| `synced` | Ready to serve queries | `ready: true` |

### Skip Mode (Header-Only Sync)

After Mithril import, the database contains UTxOs at slot X (the Mithril snapshot slot).
We cannot construct a proper chain sync checkpoint because Mithril doesn't provide the
block hash for that slot.

**The Problem:**

- Chain sync requires `Point(slot, blockHash)` for intersection
- Mithril provides only the slot number, not the block hash
- Without a valid Point, we must intersect at Origin

**The Solution: Skip Mode**

Instead of replaying all blocks from genesis (which would be slow and redundant),
we use "skip mode":

1. **Intersect at Origin** - Start chain sync from genesis
2. **Skip block fetching** - Receive headers but don't fetch/process blocks
3. **Track target slot** - The Mithril snapshot slot is the target
4. **Save checkpoint on arrival** - When we reach the target slot, we have the block hash
5. **Resume normal sync** - Continue with block fetching from that point

```
Genesis ──────────────────────────────────────── Mithril Slot ──── Tip
   │                                                   │            │
   │◀──────────── Skip Mode (headers only) ───────────▶│            │
   │              No block fetching                    │            │
   │                                                   │◀── Normal ─▶│
   │                                                   │   Sync      │
```

During skip mode:
- Headers are received from the node at ~15,000-20,000 slots/second
- Blocks are NOT fetched or processed
- Progress is shown via `headerSyncProgress`
- The database remains unchanged (already has Mithril UTxOs)

### Checkpoint Management

The base checkpoint tracks where chain sync should resume after restart:

| State | Checkpoint | Behavior |
|-------|------------|----------|
| Fresh database | None | Start Mithril bootstrap |
| After Mithril import | Origin | Start skip mode from genesis |
| After skip mode completes | `Point(mithrilSlot, blockHash)` | Resume normal sync |
| During normal operation | Updated periodically | Resume from last saved point |

**Current Limitation:** If the service restarts during skip mode (after Mithril import
but before reaching the target slot), the skip mode state is lost. See issue #76.

### Database State During Bootstrap

| Phase | Database Contents | Queryable? |
|-------|-------------------|------------|
| Downloading | Empty | No |
| Extracting | Partially populated | No |
| Syncing Headers | Complete Mithril UTxOs | Yes (but `/ready` returns false) |
| Synced | Up-to-date UTxOs | Yes |

### Interrupted Bootstrap Recovery

If the process is terminated during bootstrap (e.g., during UTxO extraction), partial
data may remain in the database. On restart, the service detects this and automatically
recovers:

1. **Detection**: A bootstrap-in-progress marker is set before streaming UTxOs
2. **On restart**: If the marker exists, incomplete bootstrap is detected
3. **Cleanup**: All database tables are wiped (including partial UTxO data)
4. **Retry**: Bootstrap restarts from the beginning

You'll see these log messages when recovery occurs:

```
Incomplete bootstrap detected, cleaning up partial data...
Armageddon cleanup started.
Armageddon cleanup completed.
Incomplete bootstrap cleanup completed, retrying bootstrap...
```

This ensures the database is never left in an inconsistent state after an interruption.

The `/ready` endpoint returns `ready: true` only when:
- `bootstrapPhase` is `synced`
- `chainTipSlot - processedSlot <= syncThreshold` (default: 100 slots)

### API Availability During Bootstrap

| Endpoint | Downloading | Extracting | Syncing Headers | Synced |
|----------|-------------|------------|-----------------|--------|
| `/metrics` | ✅ | ✅ | ✅ | ✅ |
| `/ready` | ✅ (false) | ✅ (false) | ✅ (false) | ✅ (true) |
| `/merkle-roots` | ❌ 503 | ❌ 503 | ❌ 503 | ✅ |
| `/proof/{txId}/{txIx}` | ❌ 503 | ❌ 503 | ❌ 503 | ✅ |

Data endpoints return 503 Service Unavailable until the service is fully synced.

### Monitoring Bootstrap Progress

Use the `/metrics` endpoint to monitor progress:

```bash
# Check current phase
curl -s http://localhost:8081/metrics | jq '.bootstrapPhase'

# During extraction
curl -s http://localhost:8081/metrics | jq '.extractionProgress'
# {"current": 1500000, "total": 2900000, "percent": 51.7, "rate": 850.5, "eta": 1647.2}

# During header sync
curl -s http://localhost:8081/metrics | jq '.headerSyncProgress'
# {"currentSlot": 50000000, "targetSlot": 103000000}

# Check if ready
curl -s http://localhost:8081/ready | jq '.ready'
```

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

> **Note:** Ed25519 verification protects the ancillary files (ledger state).
> Full STM certificate chain verification is still planned for the immutable files.
> See [STM verification plan](mithril-stm-ffi-plan.md) for details.

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

> **Warning:** The current implementation replays from genesis after Mithril import.
> This negates most of the bootstrap speed benefit. See issue #32 for the fix.

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
- **Environment variables**: Verification keys are read from `MITHRIL_ANCILLARY_VERIFICATION_KEY`

This verification runs automatically when `MITHRIL_ANCILLARY_VERIFICATION_KEY` is set. To skip it (not recommended):

```bash
cardano-utxo \
    --mithril-bootstrap \
    --mithril-skip-ancillary-verification \
    ...
```

To use a custom verification key via CLI:

```bash
cardano-utxo \
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

### Bootstrap Interrupted

If the process was killed during bootstrap and you see "Incomplete bootstrap detected"
on restart, this is normal behavior. The service will:

1. Clean up partial data from the previous attempt
2. Automatically retry the bootstrap from scratch

No manual intervention is required.
