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
TODO: Explore `lib/Cardano/UTxOCSMT/Mithril/Client.hs`

### Manifest File Format
TODO: Determine manifest location and format in ancillary tarball

### Ed25519 in Mithril (Rust)
- Source: `mithril-common/src/crypto_helper/ed25519.rs`
- Format: JSON-hex via `to_json_hex()` / `from_json_hex()`

### Haskell Libraries
- `crypton` (modern, maintained)
- `ed25519` package

## Tasks
- [ ] Explore current Mithril client code
- [ ] Find manifest file format in ancillary tarball
- [ ] Parse JSON-hex verification key format
- [ ] Implement Ed25519 verification
- [ ] Add `verifyAncillarySignature` function
- [ ] Add tests

## Progress Log

### Session 1 (2026-01-27)
- Created branch `feat/ed25519-ancillary-verification`
- Starting investigation...
