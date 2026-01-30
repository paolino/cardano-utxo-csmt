# Plan: Track Mithril Bootstrap Completion State (Issue 69)

## Problem

When the process is interrupted during Mithril bootstrap and restarted:
1. `checkEmptyRollbacks` checks if `RollbackPoints` is empty
2. UTxOs are being written to `KVCol`/`CSMTCol` during streaming
3. `setup` (writes Origin to RollbackPoints) is called **after** streaming completes
4. If interrupted mid-streaming: partial UTxO data exists, but RollbackPoints is still empty
5. On restart: `checkEmptyRollbacks` returns True → tries Mithril again
6. This can leave orphaned partial data or cause inconsistencies

## Solution: Bootstrap-In-Progress Marker

Add a simple marker key that is set **before** streaming UTxOs and deleted **after** bootstrap completes. On restart, if this marker exists, clean up and retry.

### Design Choice

Use the existing `ConfigCol` with a new `BootstrapInProgressKey`:
- Presence of key → bootstrap in progress (incomplete)
- Absence of key → bootstrap not started or completed successfully

This fits the existing type structure (`ConfigCol` stores `KV ConfigKey slot`).

## Implementation Steps

### 1. Extend ConfigKey (Columns.hs)

**File:** `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Columns.hs`

```haskell
data ConfigKey
    = BaseCheckpointKey
    | BootstrapInProgressKey  -- NEW
    deriving (Eq, Ord, Show)

configKeyPrism :: Prism' ByteString ConfigKey
configKeyPrism = prism' encode decode
  where
    encode BaseCheckpointKey = "base_checkpoint"
    encode BootstrapInProgressKey = "bootstrap_in_progress"  -- NEW

    decode bs
        | bs == "base_checkpoint" = Just BaseCheckpointKey
        | bs == "bootstrap_in_progress" = Just BootstrapInProgressKey  -- NEW
        | otherwise = Nothing
```

### 2. Add Query Functions (Query.hs)

**File:** `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Query.hs`

```haskell
-- | Check if bootstrap is in progress (incomplete)
isBootstrapInProgress
    :: Transaction m cf (Columns slot hash key value) op Bool
isBootstrapInProgress = do
    mval <- query ConfigCol BootstrapInProgressKey
    pure $ isJust mval

-- | Mark bootstrap as in progress (stores target slot for reference)
setBootstrapInProgress
    :: slot
    -> Transaction m cf (Columns slot hash key value) op ()
setBootstrapInProgress = insert ConfigCol BootstrapInProgressKey

-- | Clear bootstrap in progress marker
clearBootstrapInProgress
    :: Transaction m cf (Columns slot hash key value) op ()
clearBootstrapInProgress = delete ConfigCol BootstrapInProgressKey
```

Export these from Query.hs.

### 3. Modify Setup.hs - Set Marker Before Streaming

**File:** `application/Cardano/UTxOCSMT/Application/Run/Setup.hs`

In `runMithrilBootstrap`, before calling `importFromMithril`:

```haskell
runMithrilBootstrap manager downloadDir = do
    -- ... existing config setup ...

    -- Mark bootstrap as starting (with target slot from metadata)
    -- This happens BEFORE streaming UTxOs
    result <- importFromMithril
        (contramap Mithril tracer)
        mithrilConfig
        runner
        (txRunTransaction . setBootstrapInProgress)  -- NEW: callback to set marker
```

### 4. Modify Import.hs - Set Marker Before Streaming

**File:** `lib/Cardano/UTxOCSMT/Mithril/Import.hs`

In `importFromMithril`, after fetching snapshot metadata but **before** `streamToCSMT`:

```haskell
importFromMithril tracer config runner = do
    -- ... fetch snapshot, download, extract ...

    -- Set marker BEFORE streaming (with expected slot)
    liftIO $ setMarker (snapshotSlot metadata)

    -- Stream UTxOs (this is where interruption can happen)
    count <- streamToCSMT ...

    -- Return success (marker cleared in Setup.hs after full completion)
    pure ImportSuccess{...}
```

### 5. Modify Setup.hs - Check and Handle Incomplete Bootstrap

At the start of `setupDB`, check for incomplete bootstrap:

```haskell
setupDB TraceWith{..} startingPoint mithrilOpts armageddonParams runner@RunCSMTTransaction{txRunTransaction} = do
    -- Check for incomplete bootstrap FIRST
    incomplete <- txRunTransaction isBootstrapInProgress
    when incomplete $ do
        trace IncompleteBootstrapDetected
        -- Clean up partial data
        armageddon (contra Cleanup) runner armageddonParams
        -- Clear the marker
        txRunTransaction clearBootstrapInProgress
        trace IncompleteBootstrapCleaned

    -- Continue with normal setup logic
    new <- checkEmptyRollbacks runner
    -- ... rest unchanged ...
```

### 6. Clear Marker After Successful Bootstrap

In `setupDB`, after successful Mithril import and `setup` call:

```haskell
case result of
    ImportSuccess{importCheckpoint, importSlot} -> do
        setup (contra New) runner armageddonParams
        txRunTransaction $ do
            putBaseCheckpoint importCheckpoint
            clearBootstrapInProgress  -- NEW: clear marker
        return SetupResult{...}
```

### 7. Add Trace Events (Traces.hs)

**File:** `application/Cardano/UTxOCSMT/Application/Run/Traces.hs`

```haskell
data MainTraces
    = ...
    | IncompleteBootstrapDetected
    | IncompleteBootstrapCleaned
    | BootstrapMarkerSet Word64  -- target slot
    | BootstrapMarkerCleared
```

## Files to Modify

| File | Changes |
|------|---------|
| `lib/.../Columns.hs` | Add `BootstrapInProgressKey` to `ConfigKey`, update prism |
| `lib/.../Query.hs` | Add `isBootstrapInProgress`, `setBootstrapInProgress`, `clearBootstrapInProgress` |
| `application/.../Setup.hs` | Check for incomplete bootstrap on startup, set/clear markers |
| `lib/.../Import.hs` | Set marker before streaming (may need callback parameter) |
| `application/.../Traces.hs` | Add trace events for bootstrap state |

## Flow Diagram

```
setupDB
  │
  ├─► Check isBootstrapInProgress
  │     ├─ YES: armageddon + clearBootstrapInProgress + trace
  │     └─ NO: continue
  │
  ├─► checkEmptyRollbacks
  │     ├─ Empty + Mithril enabled:
  │     │     ├─► setBootstrapInProgress(targetSlot)  ◄── MARKER SET
  │     │     ├─► streamToCSMT (UTxOs)                ◄── CAN BE INTERRUPTED
  │     │     ├─► setup (writes RollbackPoints)
  │     │     ├─► putBaseCheckpoint + clearBootstrapInProgress ◄── MARKER CLEARED
  │     │     └─► return SetupResult
  │     │
  │     └─ Not empty: load existing checkpoint
  │
  └─► return SetupResult
```

## Edge Cases

1. **Crash during marker write:** RocksDB atomic operations ensure marker is fully written or not at all
2. **Multiple restarts:** Each restart detects marker, cleans up, retries - no accumulated garbage
3. **Crash after streaming but before clearing:** Data is complete, `setup` may not have run. On restart: detects incomplete, cleans up, retries. Some wasted work but consistent.
4. **Old databases:** No marker = `isBootstrapInProgress` returns False = normal operation

## Verification

1. **Unit test:** Add test for `isBootstrapInProgress`, `setBootstrapInProgress`, `clearBootstrapInProgress`
2. **Manual test:**
   - Start bootstrap with `--mithril-bootstrap`
   - Kill process during "Streaming UTxOs" phase
   - Restart - should see "IncompleteBootstrapDetected" trace
   - Should clean up and retry bootstrap
3. **Run existing tests:** `just unit` should pass

## Implementation Progress

- [x] Extend ConfigKey in Columns.hs - added `BootstrapInProgressKey` and updated prism
- [x] Add query functions in Query.hs - `isBootstrapInProgress`, `setBootstrapInProgress`, `clearBootstrapInProgress`
- [x] Add trace events in Traces.hs - `IncompleteBootstrapDetected`, `IncompleteBootstrapCleaned`
- [x] Modify Setup.hs for marker check/set/clear
- [x] Run tests - all 89 tests pass
- [x] Format and hlint - clean

Note: Import.hs modification not needed - marker is set in Setup.hs before calling `importFromMithril`
