# Architecture

This document describes the high-level architecture of the Cardano UTxO CSMT service.

## Overview

```
┌─────────────────┐     ┌──────────────────────────────────────────┐
│  Cardano Node   │     │           UTxO CSMT Service              │
│                 │     │                                          │
│  ┌───────────┐  │     │  ┌─────────────┐    ┌─────────────────┐  │
│  │ ChainSync │◄─┼─────┼──┤ ChainSync   │───►│                 │  │
│  │  Server   │  │     │  │ Client      │    │                 │  │
│  └───────────┘  │     │  └─────────────┘    │    Database     │  │
│                 │     │                     │    (RocksDB)    │  │
│  ┌───────────┐  │     │  ┌─────────────┐    │                 │  │
│  │BlockFetch │◄─┼─────┼──┤ BlockFetch  │───►│  ┌───────────┐  │  │
│  │  Server   │  │     │  │ Client      │    │  │   UTxOs   │  │  │
│  └───────────┘  │     │  └─────────────┘    │  ├───────────┤  │  │
│                 │     │                     │  │   CSMT    │  │  │
└─────────────────┘     │                     │  ├───────────┤  │  │
                        │  ┌─────────────┐    │  │ Rollback  │  │  │
                        │  │ HTTP Server │    │  │  Points   │  │  │
┌─────────────────┐     │  │             │    │  └───────────┘  │  │
│   HTTP Client   │◄────┼──┤ /metrics    │    │                 │  │
│                 │     │  │ /proof/:id  │◄───┤                 │  │
│                 │     │  │ /merkle-... │    └─────────────────┘  │
└─────────────────┘     │  └─────────────┘                         │
                        └──────────────────────────────────────────┘
```

## Components

### Chain Synchronization

The service connects to a Cardano node using the node-to-node protocol:

- **ChainSync Client**: Follows the chain tip, receiving block headers
- **BlockFetch Client**: Retrieves full block data for processing
- **KeepAlive**: Maintains the connection

Headers are queued and blocks are fetched in batches for efficiency.

### UTxO Processing

For each block, the service extracts UTxO changes:

- **Spends**: Inputs consumed by transactions (deletions)
- **Creates**: Outputs produced by transactions (insertions)

UTxO references are CBOR-encoded for consistent storage across all eras (Byron through Conway).

### Compact Sparse Merkle Tree (CSMT)

The CSMT provides efficient membership proofs:

- **Insertion**: O(log n) with path compression
- **Deletion**: O(log n) with automatic compaction
- **Proof Generation**: O(log n) inclusion proofs

The Merkle root changes with each block, providing a cryptographic commitment to the UTxO set state.

### Database (RocksDB)

Three column families store different data:

| Column | Key | Value |
|--------|-----|-------|
| UTxOs | TxIn (CBOR) | TxOut (CBOR) |
| CSMT | Path | Hash + Jump |
| Rollback Points | Slot | Changes for rollback |

Rollback points enable chain reorganization handling without full recomputation.

### HTTP API

The REST API provides:

- `GET /metrics` - Sync progress and performance metrics
- `GET /merkle-roots` - Historical merkle roots by slot
- `GET /proof/:txId/:txIx` - Inclusion proof for a UTxO

## Data Flow

1. **Block Arrival**: ChainSync receives header, BlockFetch retrieves block
2. **UTxO Extraction**: Parse transactions, extract inputs/outputs
3. **Database Update**: Apply changes atomically (deletes + inserts)
4. **CSMT Update**: Update Merkle tree, compute new root
5. **Finality Tracking**: Move finality point, prune old rollback data

## Rollback Handling

When the node reports a rollback:

1. Find the rollback point in stored history
2. Apply inverse operations to restore previous state
3. Resume following from the new chain tip

If rollback exceeds stored history (truncation), the service restarts sync from genesis.
