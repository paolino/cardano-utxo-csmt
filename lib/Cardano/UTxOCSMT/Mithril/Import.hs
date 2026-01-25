{- |
Module      : Cardano.UTxOCSMT.Mithril.Import
Description : Import UTxO data from Mithril snapshots into CSMT database

This module provides the bridge between Mithril snapshots and the CSMT
database. It orchestrates the full bootstrap process:

1. Fetch latest snapshot metadata from Mithril aggregator
2. Download and verify snapshot via mithril-client
3. Extract UTxO set from Cardano ledger state files
4. Bulk import into CSMT database
5. Set checkpoint for chain sync continuation
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

import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction
    )
import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilConfig (..)
    , MithrilError
    , MithrilTrace (..)
    , SnapshotMetadata (..)
    , downloadSnapshotHttp
    , fetchLatestSnapshot
    , renderMithrilError
    , renderMithrilTrace
    )
import Cardano.UTxOCSMT.Mithril.Extraction
    ( ExtractionError
    , ExtractionTrace
    , extractUTxOsFromSnapshot
    , renderExtractionError
    , renderExtractionTrace
    )
import Cardano.UTxOCSMT.Mithril.Streaming
    ( StreamTrace
    , defaultStreamConfig
    , renderStreamTrace
    , streamToCSMT
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Tracer (Tracer, contramap, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word64)
import Database.RocksDB (BatchOp, ColumnFamily)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))

-- | Result of Mithril import operation
data ImportResult
    = -- | Import succeeded: checkpoint, UTxO count, immutable DB path
      ImportSuccess Point Word64 FilePath
    | -- | Import failed with Mithril error
      ImportFailed MithrilError
    | -- | Import failed during extraction
      ImportExtractionFailed ExtractionError
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
    | -- | Extraction trace event
      ImportExtraction ExtractionTrace
    | -- | Streaming trace event
      ImportStreaming StreamTrace
    | -- | Progress: current count, total estimated
      ImportProgress Word64 Word64
    | -- | Import complete: total UTxOs imported, checkpoint
      ImportComplete Word64 Point
    | -- | Error during import
      ImportError MithrilError
    | -- | Extraction error
      ImportExtractionError ExtractionError

-- | Render trace for logging
renderImportTrace :: ImportTrace -> String
renderImportTrace ImportStarting =
    "Starting Mithril bootstrap import..."
renderImportTrace (ImportMithril mt) =
    renderMithrilTrace mt
renderImportTrace (ImportExtractingUTxO path) =
    "Extracting UTxO set from ledger state: " <> path
renderImportTrace (ImportExtraction et) =
    renderExtractionTrace et
renderImportTrace (ImportStreaming st) =
    renderStreamTrace st
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
renderImportTrace (ImportExtractionError err) =
    "Extraction error: " <> renderExtractionError err

{- | Import UTxO set from Mithril snapshot into CSMT database

This function orchestrates the full Mithril bootstrap process:

1. Fetches the latest snapshot metadata from the aggregator
2. Downloads and verifies the snapshot using mithril-client
3. Extracts the UTxO set from the downloaded ledger state
4. Streams UTxOs into the CSMT database
5. Returns the checkpoint for chain sync to continue from
-}
importFromMithril
    :: Tracer IO ImportTrace
    -- ^ Tracer for progress logging
    -> MithrilConfig
    -- ^ Mithril client configuration
    -> RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        hash
        ByteString
        ByteString
        IO
    -- ^ CSMT transaction runner for database operations
    -> IO ImportResult
importFromMithril tracer config runner = do
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

            downloadResult <- downloadSnapshotHttp config snapshot

            case downloadResult of
                Left err -> do
                    traceWith tracer $ ImportError err
                    pure $ ImportFailed err
                Right dbPath -> do
                    traceWith tracer
                        $ ImportMithril
                        $ MithrilDownloadComplete dbPath

                    -- Step 3: Extract UTxO and import
                    traceWith tracer $ ImportExtractingUTxO dbPath

                    extractResult <-
                        extractUTxOsFromSnapshot
                            (contramap ImportExtraction tracer)
                            dbPath
                            ( streamToCSMT
                                (contramap ImportStreaming tracer)
                                defaultStreamConfig
                                runner
                            )

                    case extractResult of
                        Left err -> do
                            traceWith tracer $ ImportExtractionError err
                            pure $ ImportExtractionFailed err
                        Right count -> do
                            let checkpoint = makeCheckpoint snapshot

                            traceWith tracer $ ImportComplete count checkpoint

                            pure $ ImportSuccess checkpoint count dbPath

{- | Create a Point from snapshot metadata for chain sync checkpoint

Note: The Mithril API doesn't provide block hash directly, only merkle root.
The chain sync will need to find intersection from the slot number.
For now, we return Origin and let chain sync handle intersection.
-}
makeCheckpoint :: SnapshotMetadata -> Point
makeCheckpoint SnapshotMetadata{} =
    -- TODO: Once we have block hash available, construct proper Point
    -- For now, return Origin - chain sync will find intersection
    Network.Point Origin
