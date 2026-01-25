{- |
Module      : Cardano.UTxOCSMT.Mithril.Import
Description : Import UTxO data from Mithril snapshots into CSMT database

This module provides the bridge between Mithril snapshots and the CSMT
database. It orchestrates the full bootstrap process:

1. Fetch latest snapshot metadata from Mithril aggregator
2. Download and verify snapshot via mithril-client
3. Extract UTxO set from Cardano immutable DB files
4. Bulk import into CSMT database
5. Set checkpoint for chain sync continuation

Note: UTxO extraction from immutable DB is marked as TODO for follow-up.
This exploration PR focuses on the infrastructure and integration points.
-}
module Cardano.UTxOCSMT.Mithril.Import
    ( -- * Import operations
      importFromMithril
    , ImportResult (..)

      -- * Tracing
    , ImportTrace (..)
    , renderImportTrace
    )
where

import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilConfig (..)
    , MithrilError
    , MithrilTrace (..)
    , SnapshotMetadata (..)
    , downloadSnapshot
    , fetchLatestSnapshot
    , renderMithrilError
    , renderMithrilTrace
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Short qualified as SBS
import Data.Text qualified as T
import Data.Word (Word64)
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Point qualified as Network.Point

-- | Result of Mithril import operation
data ImportResult
    = -- | Import succeeded, returns checkpoint and immutable DB path
      ImportSuccess Point FilePath
    | -- | Import failed with error
      ImportFailed MithrilError
    | -- | Import was skipped (e.g., no snapshots available)
      ImportSkipped String

-- | Trace events during import
data ImportTrace
    = -- | Starting Mithril import process
      ImportStarting
    | -- | Mithril client operation
      ImportMithril MithrilTrace
    | -- | Extracting UTxO from immutable DB at path
      ImportExtractingUTxO FilePath
    | -- | Progress: current count, total estimated
      ImportProgress Word64 Word64
    | -- | Import complete: total UTxOs imported, checkpoint
      ImportComplete Word64 Point
    | -- | Error during import
      ImportError MithrilError

-- | Render trace for logging
renderImportTrace :: ImportTrace -> String
renderImportTrace ImportStarting =
    "Starting Mithril bootstrap import..."
renderImportTrace (ImportMithril mt) =
    renderMithrilTrace mt
renderImportTrace (ImportExtractingUTxO path) =
    "Extracting UTxO set from immutable DB: " <> path
renderImportTrace (ImportProgress current total) =
    "Import progress: "
        <> show current
        <> " / "
        <> show total
        <> " UTxOs"
renderImportTrace (ImportComplete count _checkpoint) =
    "Mithril import complete: "
        <> show count
        <> " UTxOs imported"
renderImportTrace (ImportError err) =
    "Mithril import error: " <> renderMithrilError err

{- | Import UTxO set from Mithril snapshot into CSMT database

This function orchestrates the full Mithril bootstrap process:

1. Fetches the latest snapshot metadata from the aggregator
2. Downloads and verifies the snapshot using mithril-client
3. Extracts the UTxO set from the downloaded Cardano immutable DB
4. Bulk imports UTxOs into the CSMT database
5. Returns the checkpoint for chain sync to continue from

Note: The UTxO extraction step is currently a placeholder.
Full implementation requires parsing Cardano immutable DB format.
-}
importFromMithril
    :: Tracer IO ImportTrace
    -- ^ Tracer for progress logging
    -> MithrilConfig
    -- ^ Mithril client configuration
    -> IO ImportResult
importFromMithril tracer config = do
    traceWith tracer ImportStarting

    -- Step 1: Fetch latest snapshot metadata
    traceWith tracer
        $ ImportMithril
        $ MithrilFetchingSnapshot (mithrilAggregatorUrl config)
    snapshotResult <- fetchLatestSnapshot config

    case snapshotResult of
        Left err -> do
            traceWith tracer $ ImportError err
            pure $ ImportFailed err
        Right snapshot -> do
            let digest = snapshotDigest snapshot
                slot = snapshotBeaconSlot snapshot
                epoch = snapshotBeaconEpoch snapshot

            traceWith tracer
                $ ImportMithril
                $ MithrilSnapshotFound digest slot epoch

            -- Step 2: Download and verify snapshot
            traceWith tracer
                $ ImportMithril
                $ MithrilDownloading digest (mithrilDownloadDir config)

            downloadResult <- downloadSnapshot config digest

            case downloadResult of
                Left err -> do
                    traceWith tracer $ ImportError err
                    pure $ ImportFailed err
                Right dbPath -> do
                    traceWith tracer
                        $ ImportMithril
                        $ MithrilDownloadComplete dbPath

                    -- Step 3: Extract UTxO and import
                    -- TODO: Implement UTxO extraction from immutable DB
                    -- This requires parsing the Cardano immutable DB format
                    -- For now, we return the checkpoint for manual extraction
                    traceWith tracer $ ImportExtractingUTxO dbPath

                    let checkpoint = makeCheckpoint snapshot

                    -- Placeholder for actual UTxO count
                    traceWith tracer $ ImportComplete 0 checkpoint

                    pure $ ImportSuccess checkpoint dbPath

-- | Create a Point from snapshot metadata for chain sync checkpoint
makeCheckpoint :: SnapshotMetadata -> Point
makeCheckpoint SnapshotMetadata{snapshotBeaconSlot, snapshotBeaconBlockHash} =
    case snapshotBeaconBlockHash of
        Nothing ->
            -- If no block hash available, use Origin as fallback
            -- Chain sync will need to find intersection
            Network.Point Origin
        Just hashText ->
            -- Parse block hash and create point
            let hashBytes = hexToBytes hashText
                slot = SlotNo snapshotBeaconSlot
            in  Network.Point $ At $ Network.Point.Block slot (OneEraHash hashBytes)
  where
    -- Simple hex decoding (placeholder - should use proper decoder)
    hexToBytes :: T.Text -> SBS.ShortByteString
    hexToBytes _ = SBS.empty -- TODO: Implement proper hex decoding
