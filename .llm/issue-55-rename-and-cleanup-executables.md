# Issue #55: Rename executable and remove cardano-cli dump restore

Also fixes #59 (docker image packages wrong executable)

## Summary

Clean up executables in the project:
1. Rename `cardano-utxo-chainsync` to `cardano-utxo`
2. Remove the cardano-cli dump restore application (obsolete with Mithril bootstrap)
3. Rename `cardano-utxo-csmt-swagger` to `cardano-utxo-swagger`
4. Fix docker image and CI artifacts to use correct executable

## Changes Made

### Executable Renames

| Old Name | New Name |
|----------|----------|
| `cardano-utxo-chainsync` | `cardano-utxo` |
| `cardano-utxo-csmt-swagger` | `cardano-utxo-swagger` |

Updated in:
- `cardano-utxo-csmt.cabal` - executable sections
- `flake.nix` - packages export
- `nix/project.nix` - package definitions
- `nix/docker-image.nix` - docker image entrypoint and paths
- `hie.yaml` - HLS configuration
- `.github/workflows/CI.yaml` - main CI workflow (build targets, artifact names)
- `.github/workflows/e2e-preview.yaml` - E2E preview workflow
- `justfile` - docker image tags, added `-O0` to all cabal commands
- `docs/mithril-bootstrap.md` - documentation
- `docs/assets/mithril-demo.sh` - demo script
- `docs/assets/record-demo.sh` - demo script
- `docs/assets/mithril-bootstrap.cast` - asciinema recording

### Removed: `cardano-utxo-csmt` executable (cardano-cli dump restore)

Deleted files:
- `executables/cli/main.hs` - main entry point
- `lib/Cardano/UTxOCSMT/Bootstrap/App.hs` - bootstrap application logic

Removed from:
- `cardano-utxo-csmt.cabal` - executable section and module export
- `flake.nix` - packages export
- `nix/project.nix` - package definition
- `hie.yaml` - HLS configuration
- `justfile` - removed `dump-and-load-utxo` recipe

### Docker Image Fix (#59)

Updated `nix/docker-image.nix` to use the correct executable:
- EntryPoint now uses `cardano-utxo`
- copyToRoot paths updated to reference `cardano-utxo`

### CI Artifacts Fix (#59)

Updated `.github/workflows/CI.yaml`:
- All `nix build` and `nix bundle` commands now target `.#cardano-utxo`
- Artifact names updated (docker image, AppImage, RPM, DEB)
- Removed orphaned "Upload tarball Artifact" step (see investigation below)

### Orphaned Tarball Artifact Investigation

The CI had a warning about missing `output-linux-tarball`. Investigation of git history:

```
git log --oneline -- .github/workflows/CI.yaml
```

Found commit `f943061 bug: produce appImage instead of arx` which replaced:
```yaml
- name: Build arx
  run: nix bundle -o output-linux-tarball
```

with:
```yaml
- name: Build AppImage
  run: nix bundle --bundler github:ralismark/nix-appimage ...
```

But left behind the orphaned upload step that still referenced `output-linux-tarball`.
This step was removed in this PR.

### Additional Fixes

1. **Fixed missing tracer argument** in `downloadSnapshotHttp` calls (pre-existing issue on main)

2. **Added `-O0` to all cabal commands in justfile** - ensures consistent optimization level to avoid cache invalidation

## Commits

1. `chore: add -O0 to all cabal commands in justfile`
2. `chore: rename executable and remove cardano-cli dump restore`
3. `chore: rename cardano-utxo-csmt-swagger to cardano-utxo-swagger`
4. `fix: remove orphaned tarball artifact step`

## Build Verification

- `cabal build all -O0` - passes
- `cabal test unit-tests -O0` - all 56 tests pass
- `nix build .#docker-image` - passes locally

## Status

CI passing. Ready to merge.
