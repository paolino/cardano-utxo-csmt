# Mithril Bootstrap

Mithril enables fast bootstrapping of the Cardano UTxO CSMT database by downloading
certified snapshots instead of syncing from genesis.

## Overview

When enabled, Mithril bootstrapping:

1. Downloads the latest certified snapshot from Mithril aggregators
2. Extracts the ledger state containing the UTxO set
3. Imports UTxOs into the CSMT database
4. Continues chain sync from the snapshot point

This can reduce initial sync time from days to minutes.

## Demo

```asciinema
{
    "file": "assets/mithril-bootstrap.cast",
    "title": "Mithril Bootstrap Demo",
    "idleTimeLimit": 2,
    "poster": "npt:0:3"
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
