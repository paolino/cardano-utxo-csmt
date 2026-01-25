# Mithril Bootstrap

Mithril enables fast bootstrapping of the Cardano UTxO CSMT database by downloading
certified snapshots instead of syncing from genesis.

## Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Fetch snapshot metadata | ✅ Working | All networks (mainnet, preprod, preview) |
| HTTP download (no verification) | ✅ Default | Downloads ancillary data with ledger state |
| Find ledger state files | ✅ Working | Supports both old (.lstate) and new UTxO-HD formats |
| CLI options | ✅ Working | `--mithril-bootstrap`, `--mithril-network`, etc. |
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

**HTTP Download (current implementation)**

- Downloads directly from CDN URLs
- Fast, no external dependencies
- Suitable for development/testing

!!! warning "No Cryptographic Verification"
    The current implementation does not verify the Mithril STM certificate chain.
    This is acceptable for development but production deployments should wait for
    the FFI-based verification (see [STM verification plan](mithril-stm-ffi-plan.md)).

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

## Security Considerations

!!! warning "Current Limitation"
    The current implementation uses unverified HTTP download. Snapshots are
    fetched from Mithril CDN without cryptographic verification of the STM
    certificate chain.

- **Production**: Wait for FFI-based STM verification (planned)
- **Development**: HTTP download is acceptable for testing
- Snapshots are signed by Cardano stake pool operators using STM
- Future: Certificate chain verification via Rust FFI bindings

See the [Mithril STM FFI plan](mithril-stm-ffi-plan.md) for the verification
implementation roadmap.

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
