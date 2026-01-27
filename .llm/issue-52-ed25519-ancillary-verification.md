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

### Manifest File Format (DISCOVERED)
**File**: `ancillary_manifest.json` in tarball root
**Source**: `mithril-cardano-node-internal-database/src/entities/ancillary_files_manifest.rs`

```json
{
  "data": {
    "ledger/12345": "sha256-hex-hash",
    "volatile/blocks/...": "sha256-hex-hash"
  },
  "signature": "ed25519-signature-hex-64-bytes"
}
```

**Verification process** (from `mithril-client/src/utils/ancillary_verifier.rs`):
1. Parse `ancillary_manifest.json` as `AncillaryFilesManifest`
2. Verify file hashes: for each (path, hash) in `data`, compute SHA256 of file and compare
3. Compute manifest hash: `SHA256(path1_bytes || hash1_bytes || path2_bytes || hash2_bytes || ...)`
   - Keys are sorted (BTreeMap), so order is deterministic
4. Verify Ed25519 signature over the manifest hash

### Ed25519 Key/Signature Format (DISCOVERED)
**Source**: `mithril-common/src/crypto_helper/codec/json_hex.rs`

**JSON-hex format**:
- Serialize key bytes as JSON array: `[23, 27, 112, 6, ...]`
- Hex-encode the JSON string
- Example verification key: `5b32332c32372c3131322c...5d` (hex of `[23,27,112,...]`)

**Key sizes**:
- Ed25519 verification key: 32 bytes → JSON array of 32 u8 → ~100 hex chars
- Ed25519 signature: 64 bytes → JSON array of 64 u8 → ~200 hex chars

**Library**: `ed25519_dalek` in Rust → `crypton` in Haskell

### Haskell Libraries
- `crypton` (modern, maintained) - preferred
- `ed25519` package

## Tasks
- [x] Explore current Mithril client code
- [x] Find manifest file format in ancillary tarball
- [x] Understand JSON-hex verification key format
- [x] Implement JSON-hex decoding in Haskell
- [x] Implement Ed25519 verification
- [x] Add `verifyAncillaryManifest` function
- [ ] Integrate into `downloadSnapshotHttp` (IN PROGRESS)
- [ ] Add tests

## Progress Log

### Session 1 (2026-01-27)
- Created branch `feat/ed25519-ancillary-verification`
- Explored `Mithril/Client.hs` - found `downloadSnapshotHttp` function
- Set up worktree at `/code/cardano-utxo-csmt-issue-52`
- No crypto dependencies in project yet

### Session 2 (2026-01-27)
- Cloned Mithril repo to `/tmp/mithril-repo`
- **Found manifest format**: `AncillaryFilesManifest` in `ancillary_files_manifest.rs`
  - File: `ancillary_manifest.json`
  - Structure: `{ data: Map<Path, SHA256Hex>, signature: Ed25519Hex }`
- **Found verification logic**: `AncillaryVerifier` in `ancillary_verifier.rs`
  - Verify file hashes
  - Compute manifest hash (SHA256 of concatenated paths and hashes)
  - Verify Ed25519 signature
- **Found key format**: JSON-hex encoding (JSON array of bytes, hex-encoded)
- **Ed25519 library**: `ed25519_dalek` (Rust) → `crypton` (Haskell)

### Session 3 (2026-01-27)
- **AncillaryVerifier module fully implemented** at `lib/Cardano/UTxOCSMT/Mithril/AncillaryVerifier.hs`
  - `parseJsonHex` - decodes Mithril's JSON-hex format
  - `parseVerificationKey` - parses Ed25519 public key from JSON-hex
  - `computeManifestHash` - SHA256 of sorted path||hash pairs
  - `verifySignature` - Ed25519 signature verification
  - `verifyAncillaryManifest` - full verification flow
  - `AncillaryVerificationError` - error sum type
- **crypton already added** to cabal dependencies
- **Started Client.hs integration**:
  - Added import for AncillaryVerifier
  - Added `mithrilAncillaryVk :: Maybe AncillaryVerificationKey` to `MithrilConfig`
- **Found network-specific keys** in Mithril repo:
  - Preview: `5b3138392c3139322c3231362c...` (from `mithril-client-cli/config/preview.json`)
  - Mainnet/Preprod: not yet configured (ancillary feature may be newer)

### Remaining Work
1. Update `defaultMithrilConfig` to include ancillary key
2. Add `ancillaryVkForNetwork` function (like `genesisVkForNetwork`)
3. Add `MithrilVerificationFailed` error variant
4. Call `verifyAncillaryManifest` after extraction in `downloadSnapshotHttp`
5. Update `renderMithrilError` for new error variant
6. Add unit tests for AncillaryVerifier module
7. Add integration test with real manifest (if available)

### Network Ancillary Verification Keys
From `mithril-client-cli/config/`:
```
preview: 5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c3231302c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c31342c3232382c3136382c3132392c38332c3133362c33365d
mainnet: not yet available
preprod: not yet available
```
