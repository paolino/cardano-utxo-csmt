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
