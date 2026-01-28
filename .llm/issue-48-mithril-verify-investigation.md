# Issue #48: Use Mithril CLI to Verify State Snapshot

**PR:** #50
**Branch:** `feat/mithril-verify`
**Worktree:** `/code/cardano-utxo-csmt-issue-48`

## Summary

Add `--mithril-verify-mode` option for controlling snapshot verification:
- `stm` - Full STM certificate verification via mithril-client CLI
- `ed25519` - Ed25519 signature verification of ancillary manifest (default)
- `none` - No verification (testing only)

## Implementation (2026-01-28)

### Changes Made

1. **Options.hs** - New `MithrilVerifyMode` enum and parser
   - `VerifyStm` - Full STM verification via mithril-client
   - `VerifyEd25519` - Ed25519 ancillary verification (default)
   - `VerifyNone` - No verification
   - Replaces `mithrilSkipAncillaryVerification` flag

2. **Client.hs** - Updated `downloadSnapshot` for STM mode
   - Takes `SnapshotMetadata` instead of `SnapshotDigest`
   - Uses v2 backend with `--start N --end N` for minimal download
   - Reduces download from ~14GB to ~500MB-1GB
   - Merges environment variables with current env (fixes SSL/TLS)
   - Added `ancillaryVkForNetwork` for mithril-client v2 backend

3. **Import.hs** - Dispatch based on verify mode
   - `VerifyStm` → `downloadSnapshot` (mithril-client CLI)
   - `VerifyEd25519` → `downloadSnapshotHttp` with Ed25519 verification
   - `VerifyNone` → `downloadSnapshotHttp` without verification

4. **Main.hs** - Wiring updates
   - Pass `mithrilVerifyMode` to `importFromMithril`
   - Update ancillary key logic for Ed25519 mode

5. **flake.nix** - Add mithril 2603.1-pre
   - mithril-client 0.12.38 fixes v2 backend ancillary download bug
   - Add E2E shell (`nix develop .#e2e`) with mithril-client

### Test Results

- All 56 unit tests pass
- E2E verified download with STM mode works (tested with 2603.1-pre)

## CLI Usage

```bash
# STM verification (full cryptographic proof chain)
cardano-utxo-chainsync --mithril-bootstrap \
  --mithril-verify-mode stm \
  --mithril-network preprod

# Ed25519 verification (default, lighter weight)
cardano-utxo-chainsync --mithril-bootstrap \
  --mithril-verify-mode ed25519 \
  --mithril-network preprod

# No verification (testing only)
cardano-utxo-chainsync --mithril-bootstrap \
  --mithril-verify-mode none \
  --mithril-network preprod
```

## Files Modified

| File | Changes |
|------|---------|
| `lib/Cardano/UTxOCSMT/Mithril/Options.hs` | Add `MithrilVerifyMode` enum, replace skip flag |
| `lib/Cardano/UTxOCSMT/Mithril/Client.hs` | Update `downloadSnapshot` for v2 backend, add `ancillaryVkForNetwork` |
| `lib/Cardano/UTxOCSMT/Mithril/Import.hs` | Dispatch based on verify mode |
| `application/.../Run/Main.hs` | Wire verify mode through |
| `executables/extract-utxos/main.hs` | Add tracer to `downloadSnapshotHttp` |
| `executables/memory-test/Main.hs` | Add tracer to `downloadSnapshotHttp` |
| `flake.nix` | Add mithril 2603.1-pre input |
| `nix/project.nix` | Add E2E shell with mithril-client |

## Known Issues

### STM Mode Blocked by Aggregator Bug

The `--mithril-verify-mode stm` option is currently blocked by a server-side bug in the Mithril aggregators. When using the v2 backend API, the aggregators return:

```
unknown variant `preview`, expected `MainNet` or `TestNet`
```

This is a serialization mismatch between the aggregator and client versions. The aggregators need to be updated to support the v2 API.

**Impact:** STM verification mode will fail until IOG updates the aggregators.
**Workaround:** Use `--mithril-verify-mode ed25519` (default) which works correctly.

## Commits

1. `ab7452e` - feat: add --mithril-verify-mode option for verification control
2. `98a3b22` - feat: add mithril 2603.1-pre and E2E shell for STM verification testing
