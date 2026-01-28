# Issue 61: Update generateInclusionProof API

## Summary

Update haskell-csmt source-repository-package from `e33d7110ca9e708f367ca19c69020fc853be4980` to HEAD (`308d2232d9db8064698460e0aebc101dadbd6fe4`).

## Progress

- [x] Created issue #61
- [x] Created worktree at `/code/cardano-utxo-csmt-issue-61`
- [x] Updated cabal.project with new commit and SHA256 hash
- [x] Fix breaking API changes
- [x] Run tests (passed, except disk space issues on Mithril E2E tests)
- [x] Create PR

## API Changes

### `generateInclusionProof` signature changed

**Old:** `FromKV k v a -> Selector d Key (Indirect a) -> k -> Transaction m cf d ops (Maybe ByteString)`

**New:** `FromKV k v Hash -> Selector d k v -> Selector d Key (Indirect Hash) -> k -> Transaction m cf d ops (Maybe (v, ByteString))`

Changes:
- Now takes an additional `Selector d k v` argument for the KV column
- Returns `Maybe (v, ByteString)` instead of `Maybe ByteString` (value included with proof)

**Affected files:**
- `application/Cardano/UTxOCSMT/Application/Run/Main.hs`
- `test/Cardano/UTxOCSMT/HTTP/ServerSpec.hs`

### `downloadSnapshotHttp` signature changed (incidental)

Now requires a `Tracer IO MithrilTrace` as the first argument.

**Affected files:**
- `executables/memory-test/Main.hs`
- `executables/extract-utxos/main.hs`

## cabal.project Changes

```diff
source-repository-package
  type: git
  location: https://github.com/paolino/haskell-csmt
-  tag: e33d7110ca9e708f367ca19c69020fc853be4980
-  --sha256: sha256-krDEh3RM6ZJ5c4BMdbFd3/UX2TX5x/rafUmz/vpR/XA=
+  tag: 308d2232d9db8064698460e0aebc101dadbd6fe4
+  --sha256: sha256-yReAFtoHnGkXBQu3h7ESIDCgjT34bqWm5WeKDDTzhjc=
```
