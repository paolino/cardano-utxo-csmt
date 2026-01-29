# Issue #51: Expose extraction and header sync progress via HTTP metrics

## Problem Summary

During startup, two long-running phases have no HTTP API visibility:
1. **Mithril UTxO extraction** (~1 hour for ~2.9M UTxOs)
2. **Header sync after Mithril** (variable time to catch up)

## Proposed API Addition

```json
{
  "bootstrapPhase": "extracting" | "syncing_headers" | "synced" | null,
  "extractionProgress": {
    "current": 1300000,
    "rate": 894
  },
  "headerSyncProgress": {
    "currentSlot": 51682964,
    "targetSlot": 102859682
  }
}
```

## Codebase Investigation

### 1. MetricsEvent Type

**Location:** `lib/Cardano/UTxOCSMT/Application/Metrics.hs:134-147`

```haskell
data MetricsEvent
    = BlockFetchEvent EventQueueLength
    | UTxOChangeEvent
    | BlockInfoEvent Header
    | MerkleRootEvent Hash
    | BaseCheckpointEvent Point
    | ChainTipEvent SlotNo
```

**Action:** Add new event variants:
- `ExtractionProgressEvent Word64` (current count)
- `HeaderSyncProgressEvent SlotNo SlotNo` (current, target)
- `BootstrapPhaseEvent BootstrapPhase`

### 2. Metrics Type

**Location:** `lib/Cardano/UTxOCSMT/Application/Metrics.hs:222-234`

Current fields: `averageQueueLength`, `maxQueueLength`, `utxoChangesCount`, `lastBlockPoint`, `utxoSpeed`, `blockSpeed`, `currentEra`, `currentMerkleRoot`, `baseCheckpoint`, `chainTipSlot`

**Action:** Add new fields:
- `bootstrapPhase :: Maybe BootstrapPhase`
- `extractionProgress :: Maybe ExtractionProgress`
- `headerSyncProgress :: Maybe HeaderSyncProgress`

### 3. Fold Architecture

Uses `Control.Foldl` for composable accumulators.

**Master fold:** `metricsFold` at line 330 combines all individual folds via Applicative.

**Pattern for new folds:**
```haskell
extractionProgressFold :: Fold TimedMetrics (Maybe ExtractionProgress)
headerSyncProgressFold :: Fold TimedMetrics (Maybe HeaderSyncProgress)
bootstrapPhaseFold :: Fold TimedMetrics (Maybe BootstrapPhase)
```

### 4. HTTP Endpoint

**Location:** `http/Cardano/UTxOCSMT/HTTP/Server.hs:50-52`

```haskell
metricsHandler = do
    r <- liftIO getMetrics
    maybe (throwError err404) return r
```

Reads from `TVar (Maybe Metrics)` populated by metrics tracer.

### 5. Existing Progress Types

**HeaderSkipProgress:** `lib/Cardano/UTxOCSMT/Application/BlockFetch.hs:78-84`
```haskell
data HeaderSkipProgress = HeaderSkipProgress
    { skipCurrentSlot :: SlotNo
    , skipTargetSlot :: SlotNo
    }
```

**ExtractionTrace:** `lib/Cardano/UTxOCSMT/Mithril/Extraction.hs:120-133`
- `ExtractionProgress Word64` - UTxOs extracted so far

**ImportTrace:** `lib/Cardano/UTxOCSMT/Mithril/Import.hs:79-98`
- `ImportProgress Word64 Word64` - current, total

### 6. Swagger Schema

**Location:** `docs/assets/swagger.json`

Auto-generated from `ToSchema` instances in:
- `Metrics.hs:276-313`
- `API.hs:139-176`

**Regenerate with:** `just update-swagger`

## Implementation Plan

### Step 1: Define new types in Metrics.hs

```haskell
data BootstrapPhase
    = Extracting
    | SyncingHeaders
    | Synced
    deriving (Show, Eq, Generic)

data ExtractionProgress = ExtractionProgress
    { extractionCurrent :: Word64
    , extractionRate :: Double
    }
    deriving (Show, Eq, Generic)

data HeaderSyncProgress = HeaderSyncProgress
    { headerCurrentSlot :: SlotNo
    , headerTargetSlot :: SlotNo
    }
    deriving (Show, Eq, Generic)
```

### Step 2: Add MetricsEvent variants

```haskell
data MetricsEvent
    = ...existing...
    | ExtractionProgressEvent Word64
    | HeaderSyncProgressEvent SlotNo SlotNo
    | BootstrapPhaseEvent BootstrapPhase
```

### Step 3: Add fields to Metrics and create folds

Add fields, create fold functions, update `metricsFold`.

### Step 4: Add ToJSON/ToSchema instances

For all new types.

### Step 5: Wire tracers

- In Mithril extraction: emit `ExtractionProgressEvent`
- In header skip logic: emit `HeaderSyncProgressEvent`
- At phase transitions: emit `BootstrapPhaseEvent`

### Step 6: Update swagger

Run `just update-swagger` after all changes.

## Key Files to Modify

1. `lib/Cardano/UTxOCSMT/Application/Metrics.hs` - Types, events, folds, JSON/Schema
2. `lib/Cardano/UTxOCSMT/Mithril/Import.hs` - Emit extraction events
3. `lib/Cardano/UTxOCSMT/Application/BlockFetch.hs` - Emit header sync events
4. `application/Cardano/UTxOCSMT/Application/Run/Application.hs` - Phase transitions
5. `docs/assets/swagger.json` - Regenerate

## Questions Resolved

1. **Extraction rate**: Calculated in the fold using `speedOfSomeEvent`, same pattern as `utxoSpeed`.
2. **BootstrapPhaseEvent Synced**: Emitted via `onSkipComplete` callback in `BlockFetch.hs` when skip phase ends, or directly in `Main.hs` if no skip needed.
3. **Total field**: Not included - would require knowing total upfront which isn't available during streaming extraction.

## Implementation Status

All steps complete:

- [x] Step 1: Define types (`BootstrapPhase`, `ExtractionProgress`, `HeaderSyncProgress`)
- [x] Step 2: Add `MetricsEvent` variants
- [x] Step 3: Add `Metrics` fields and folds
- [x] Step 4: Add `ToJSON`/`ToSchema` instances
- [x] Step 5: Wire tracers at emission points
- [x] Step 6: Regenerate swagger
- [x] Tests passing (56 examples, 0 failures)

## Files Modified

1. `lib/Cardano/UTxOCSMT/Application/Metrics.hs` - New types, events, folds, JSON/Schema
2. `lib/Cardano/UTxOCSMT/Application/BlockFetch.hs` - Added `onSkipComplete` callback
3. `application/Cardano/UTxOCSMT/Application/Run/Application.hs` - Header sync metrics, phase events
4. `application/Cardano/UTxOCSMT/Application/Run/Main.hs` - Extraction events via `stealMetricsEvent`
5. `test/Cardano/UTxOCSMT/HTTP/ServerSpec.hs` - Updated test fixtures
6. `docs/assets/swagger.json` - Regenerated
7. `lib/Cardano/UTxOCSMT/Mithril/Extraction.hs` - Added counting phase, progress events
8. `lib/Data/Tracer/Timestamps.hs` - Filter empty log messages
9. `run/*.sh` - Runner scripts for local testing

## Session 2 Updates (2026-01-28)

### Issues Found and Fixed

1. **API not available during bootstrap**: Moved API server startup before `setupDB`
2. **Extraction rate always 0**: Removed 10k batching from progress events
3. **Percentage null**: Added pre-counting pass for indefinite-length CBOR maps
4. **Phase stuck at "counting"**: Added `ExtractionStreamStarting` trace
5. **Log explosion**: 4M lines for 100k UTxOs - progress logged every item!
   - Fixed by filtering in `addTimestampsTracer` (empty strings skipped)
   - Fixed by batching render output (log every 100k, not every 1)
   - Fixed by handling empty nested renders in `renderMainTraces`
6. **Disk full handling**: Created issue #67 - should fail, not silently corrupt

### New Bootstrap Phases

```haskell
data BootstrapPhase
    = Downloading
    | Counting
    | Extracting
    | SyncingHeaders
    | Synced
```

### New Extraction Traces

```haskell
data ExtractionTrace
    = ...
    | ExtractionCounting           -- Start of counting phase
    | ExtractionCountingProgress Word64  -- Progress during counting
    | ExtractionDecodedState Word64      -- Total counted
    | ExtractionStreamStarting     -- Start of extraction phase
    | ExtractionProgress Word64    -- Progress during extraction
```

### Remaining Work

- [ ] **CRITICAL: Fix metricsTracer fold not updating** - intercept works (1M events forwarded) but fold doesn't accumulate. Issue is in `metricsTracer` or `metricsFold` in Metrics.hs
- [ ] Download progress (blocks count) - requires parsing mithril-client stdout
- [ ] Format code
- [ ] Final testing
- [ ] Create PR

### Debug Findings

1. **Intercept is working**: Debug output showed 1+ million events forwarded to metricsEvent tracer
2. **Phase stuck at "downloading"**: Despite BootstrapPhaseEvent Counting being forwarded, metrics show "downloading"
3. **Fold accumulation suspect**: The `bootstrapPhaseFold` uses `handles (timedEventL . _BootstrapPhaseEvent) Fold.last` - needs investigation
4. **metricsTracer architecture**:
   - Events → TQueue → accumulator thread (every 100ms) → fold → TVar
   - Output thread (every 1s) → extract from fold → metricsOutput
   - Issue likely in accumulator or fold step

## Session 3 Updates (2026-01-29)

### Module Split Refactoring

The Metrics module (~570 lines) was split into three well-organized modules:

#### New Module Structure

1. **`lib/Control/Foldl/Extra.hs`** (49 lines)
   - Generic fold utilities: `speedoMeter`, `averageOverWindow`
   - Reusable beyond just metrics
   - Module docs with haddock

2. **`lib/Cardano/UTxOCSMT/Application/Metrics/Types.hs`** (363 lines)
   - All data types: `BootstrapPhase`, `MetricsEvent`, `ExtractionProgress`, `HeaderSyncProgress`, `Metrics`, `MetricsParams`
   - ToJSON/ToSchema instances
   - Prisms for `MetricsEvent` (via Template Haskell)
   - Render utilities: `renderBlockPoint`, `renderPoint`

3. **`lib/Cardano/UTxOCSMT/Application/Metrics.hs`** (256 lines)
   - Re-exports everything from `Metrics.Types` (API unchanged)
   - Contains metrics-specific folds and `metricsTracer`
   - Imports utilities from `Control.Foldl.Extra`

#### Benefits

- **Separation of concerns**: Types vs logic vs generic utilities
- **Reusability**: `Control.Foldl.Extra` can be used elsewhere
- **Maintainability**: Smaller, focused modules
- **Backward compatibility**: Existing imports unchanged

#### Verification

- Build passes with no warnings
- hlint passes with no hints
- All existing imports work unchanged

#### Commit

```
d626d54 refactor: split Metrics module into Types and Foldl.Extra
```

### Remaining Work

- [x] **CRITICAL: Fix metricsTracer fold not updating** - intercept works (1M events forwarded) but fold doesn't accumulate
- [ ] Download progress (blocks count) - requires parsing mithril-client stdout
- [ ] Final testing
- [ ] Create PR
