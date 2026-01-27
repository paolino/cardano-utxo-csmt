# Issue #52: Ed25519 Verification for Ancillary Downloads

## Date: 2026-01-27

## Objective
Implement lightweight Ed25519 signature verification for Mithril ancillary files (ledger state) without requiring full mithril-client or downloading 14GB immutable history.

## Background
- Full Mithril download: ~14GB (immutable + ancillary)
- We only need ancillary (ledger state) for UTxO extraction
- Ancillary files use Ed25519 signatures (not STM)
- Verification key: `ANCILLARY_VERIFICATION_KEY`

## Investigation

### Current Mithril Implementation
Explored `lib/Cardano/UTxOCSMT/Mithril/Client.hs`:
- `downloadSnapshotHttp` downloads ancillary tarball without verification
- Uses `snapshotAncillaryLocations` from metadata for download URLs
- Extracts to `mithrilDownloadDir` using `tar -I zstd`
- No crypto dependencies currently in cabal file

### Manifest File Format
TODO: Determine manifest location and format in ancillary tarball
- Need to download actual ancillary tarball and inspect contents
- Or find Mithril source that creates the manifest

### Ed25519 in Mithril (Rust)
- Source: `mithril-common/src/crypto_helper/ed25519.rs`
- Format: JSON-hex via `to_json_hex()` / `from_json_hex()`

### Haskell Libraries
- `crypton` (modern, maintained) - preferred
- `ed25519` package

## Tasks
- [x] Explore current Mithril client code
- [ ] Find manifest file format in ancillary tarball
- [ ] Parse JSON-hex verification key format
- [ ] Implement Ed25519 verification
- [ ] Add `verifyAncillarySignature` function
- [ ] Add tests

## Progress Log

### Session 1 (2026-01-27)
- Created branch `feat/ed25519-ancillary-verification`
- Explored `Mithril/Client.hs` - found `downloadSnapshotHttp` function
- Set up worktree at `/code/cardano-utxo-csmt-issue-52`
- No crypto dependencies in project yet

### Next Steps
1. Download a real ancillary tarball to inspect manifest format
2. Find Mithril source code for ancillary signing/verification
3. Add `crypton` dependency and implement verification
