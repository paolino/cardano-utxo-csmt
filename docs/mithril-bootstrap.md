# Mithril Bootstrap

Mithril enables fast bootstrapping of the Cardano UTxO CSMT database by downloading
certified snapshots instead of syncing from genesis.

## Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Fetch snapshot metadata | ✅ Working | All networks (mainnet, preprod, preview) |
| HTTP download (no verification) | ✅ Working | Downloads ancillary data with ledger state |
| Find ledger state files | ✅ Working | Supports both old (.lstate) and new UTxO-HD formats |
| CLI options | ✅ Working | `--mithril-bootstrap`, `--mithril-network`, etc. |
| UTxO extraction from ledger state | ❌ TODO | Requires UTxO-HD API support (MapKind parameter) |
| Import UTxOs to CSMT | ❌ TODO | Blocked by extraction |
| Verified download (mithril-client) | ❌ TODO | Binary not available in nix shell |

## Overview

When fully implemented, Mithril bootstrapping will:

1. Downloads the latest certified snapshot from Mithril aggregators
2. Extracts the ledger state containing the UTxO set
3. Imports UTxOs into the CSMT database
4. Continues chain sync from the snapshot point

This will reduce initial sync time from days to minutes.

**Current state**: Steps 1-2 work (download and locate ledger state). Steps 3-4 are blocked
pending UTxO-HD API support in `ouroboros-consensus-cardano`.

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
    --mithril-bootstrap \
    --mithril-network preview \
    --csmt-db-path /path/to/db \
    --node-name preview-node.world.dev.cardano.org \
    --port 30002 \
    --network-magic 2
```

## CLI Options

| Option | Description | Default |
|--------|-------------|---------|
| `--mithril-bootstrap` | Enable Mithril bootstrapping | `false` |
| `--mithril-network NETWORK` | Network: `mainnet`, `preprod`, `preview` | `mainnet` |
| `--mithril-aggregator URL` | Override aggregator URL | Network default |
| `--mithril-client-path PATH` | Path to mithril-client binary | `mithril-client` |
| `--mithril-download-dir DIR` | Directory for downloads | Temp directory |

## Networks

| Network | Aggregator URL |
|---------|---------------|
| Mainnet | `https://aggregator.release-mainnet.api.mithril.network/aggregator` |
| Preprod | `https://aggregator.release-preprod.api.mithril.network/aggregator` |
| Preview | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` |

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

### 2. Download and Extract

Two download methods are available:

**HTTP Download (no verification)**
- Downloads directly from CDN URLs
- Faster, no external dependencies
- Suitable for development/testing

**Verified Download (requires mithril-client)**
- Uses `mithril-client` CLI
- Verifies STM certificate chain
- Recommended for production

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

### 4. UTxO Extraction (TODO)

Extracting UTxOs from the ledger state requires decoding the `ExtLedgerState` type
from `ouroboros-consensus-cardano`. Recent versions use the UTxO-HD API which adds
a `MapKind` type parameter. This decoding is not yet implemented.

### 5. CSMT Import (TODO)

Once UTxOs are extracted, they will be streamed into the CSMT database using the
same CBOR encoding as the chain sync module. The `Streaming.hs` module provides
the infrastructure for batched imports with progress reporting.

## Security Considerations

- **Production**: Use `mithril-client` for verified downloads
- **Development**: HTTP download is acceptable for testing
- Snapshots are signed by Cardano stake pool operators
- Certificate chain verification ensures snapshot integrity

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

Preview network ancillary data is ~400MB.
Mainnet is significantly larger.
Ensure sufficient disk space and bandwidth.
