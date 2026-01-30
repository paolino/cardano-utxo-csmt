# Issue #72: Remove Partial Option and Ensure Run Scripts Reach Network Tip

## Summary

Remove the `PartialHistory` type (always use `Complete` behavior), remove dead `Limit` code, and update run scripts with sync-waiting guidance.

## Files to Modify

| File | Changes |
|------|---------|
| `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Update.hs` | Remove `PartialHistory` type, update 3 functions |
| `lib/Cardano/UTxOCSMT/Application/Database/RocksDB.hs` | Remove import, update 2 function signatures |
| `application/Cardano/UTxOCSMT/Application/Run/Main.hs` | Remove import, remove `Partial` argument |
| `test/Cardano/UTxOCSMT/Application/Database/RocksDBSpec.hs` | Remove import, remove `Complete` argument |
| `test/Cardano/UTxOCSMT/HTTP/ServerSpec.hs` | Remove import, remove `Complete` argument |
| `lib/Cardano/UTxOCSMT/Application/Options.hs` | Remove dead `Limit` type |
| `run/cardano-utxo.sh` | Add sync-waiting function |

## Implementation Steps

### Step 1: Update `Update.hs`

**File**: `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Update.hs`

1. Remove from exports (line 3): `, PartialHistory (..)`
2. Remove type definition (line 80): `data PartialHistory = Complete | Partial`
3. Update `newState` signature (line 104): remove `-> PartialHistory` parameter
4. Update `newState` function body: remove `partiality` from args and call to `mkUpdate`
5. Update `forwardTip` signature (line 133): remove `-> PartialHistory` parameter
6. Update `forwardTip` function body (lines 156-161): replace pattern match with:
   ```haskell
   Nothing ->
       error "forwardTip: cannot invert Delete operation, key not found"
   ```
7. Update `mkUpdate` signature (line 277): remove `-> PartialHistory` parameter
8. Update `mkUpdate` function body: remove `partiality` from args and call to `forwardTip`

### Step 2: Update `RocksDB.hs`

**File**: `lib/Cardano/UTxOCSMT/Application/Database/RocksDB.hs`

1. Remove import (line 36): `PartialHistory`
2. Update `newRocksDBState` signature (line 117): remove `-> PartialHistory`
3. Update `newRocksDBState` function body (line 124): remove `partiality` argument
4. Update call to `newState` (line 127): remove `partiality` argument
5. Update `createUpdateState` signature (line 133): remove `-> PartialHistory`
6. Function body unchanged (it delegates to `newState`)

### Step 3: Update `Main.hs`

**File**: `application/Cardano/UTxOCSMT/Application/Run/Main.hs`

1. Remove import (lines 15-17): `PartialHistory (..)`
2. Update call to `createUpdateState` (lines 187-193): remove `Partial` argument

**Before:**
```haskell
(state, slots) <-
    createUpdateState
        (contra Update)
        Partial
        slotHash
        armageddonParams
        runner
```

**After:**
```haskell
(state, slots) <-
    createUpdateState
        (contra Update)
        slotHash
        armageddonParams
        runner
```

### Step 4: Update Test Files

**File**: `test/Cardano/UTxOCSMT/Application/Database/RocksDBSpec.hs`

1. Remove from import: `PartialHistory (..)`
2. Remove `Complete` argument from `mkUpdate` call

**File**: `test/Cardano/UTxOCSMT/HTTP/ServerSpec.hs`

1. Remove from import: `PartialHistory (..)`
2. Remove `Complete` argument from `mkUpdate` call

### Step 5: Remove Dead `Limit` Type

**File**: `lib/Cardano/UTxOCSMT/Application/Options.hs`

1. Remove from exports (line 5): `, Limit (..)`
2. Remove type definition (lines 77-79):
   ```haskell
   -- | A limit on the number of blocks to sync
   newtype Limit = Limit {limit :: Word32}
       deriving newtype (Show, Read, Eq, Ord, Enum)
   ```

### Step 6: Update Run Script

**File**: `run/cardano-utxo.sh`

Add sync-waiting function and guidance:

```bash
# Function to wait for sync completion
wait_for_sync() {
    local api_url="http://localhost:$API_PORT/ready"
    echo "Waiting for sync to complete..."
    while true; do
        response=$(curl -s "$api_url" 2>/dev/null || echo '{"ready":false}')
        ready=$(echo "$response" | jq -r '.ready // false')
        if [ "$ready" = "true" ]; then
            echo "Service is synced!"
            break
        fi
        slots_behind=$(echo "$response" | jq -r '.slotsBehind // "unknown"')
        echo "Still syncing... slots behind: $slots_behind"
        sleep 10
    done
}

# Usage: To wait for sync after starting, run:
#   wait_for_sync
```

## Verification

1. **Build**: `just build`
2. **Unit tests**: `just unit`
3. **Format check**: `just format && git diff --exit-code`
4. **Hlint**: `just hlint`
5. **Manual test**: Start service with Mithril bootstrap and verify:
   - Service syncs from Mithril snapshot to network tip
   - `/ready` endpoint returns `ready: true` when synced
   - No errors for delete operations (confirms Mithril starting point is correct)

## Notes

- The `/ready` endpoint logic is already correct (compares `chainTipSlot` with `processedSlot`)
- Default `syncThreshold` is 100 slots (~33 minutes)
- Removing `Partial` behavior assumes Mithril bootstrap correctly sets the starting point

## Implementation Complete

**Date**: 2026-01-30

All changes implemented successfully:
- Build passes
- All 89 unit tests pass
- Code formatted with fourmolu
- No hlint warnings

**Changes summary**:
- Removed `PartialHistory` type and `Partial`/`Complete` constructors
- Updated `newState`, `forwardTip`, `mkUpdate` signatures (removed `PartialHistory` param)
- Updated `newRocksDBState`, `createUpdateState` signatures
- Removed dead `Limit` type from Options.hs
- Added `wait_for_sync` function to run script
