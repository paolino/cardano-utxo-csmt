module Cardano.UTxOCSMT.Application.Run.Traces
    ( MainTraces (..)
    , renderMainTraces
    , stealMetricsEvent
    )
where

-- \|
-- Module      : Cardano.UTxOCSMT.Application.Run.Traces
-- Description : Trace types and rendering for the main application
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module defines the trace types used for logging throughout the main
-- application lifecycle, including startup, database operations, HTTP services,
-- and Mithril bootstrap events.

import CSMT.Hashes (Hash)
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonTrace
    , renderArmageddonTrace
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( UpdateTrace (UpdateForwardTip)
    , renderUpdateTrace
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( BootstrapPhase (..)
    , MetricsEvent (..)
    )
import Cardano.UTxOCSMT.Application.Run.Application
    ( ApplicationTrace
    , renderApplicationTrace
    )
import Cardano.UTxOCSMT.Mithril.Extraction
    ( ExtractionTrace
        ( ExtractionCounting
        , ExtractionDecodedState
        , ExtractionProgress
        , ExtractionStreamStarting
        )
    )
import Cardano.UTxOCSMT.Mithril.Import
    ( ImportTrace (..)
    , renderImportTrace
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)

{- | Main application trace types for logging various events during
the application lifecycle.
-}
data MainTraces
    = -- | Application is starting up
      Boot
    | -- | Database already contains data at the given checkpoint
      NotEmpty Point
    | -- | New database setup event (armageddon initialization)
      New ArmageddonTrace
    | -- | Database update event (block processing)
      Update (UpdateTrace Point Hash)
    | -- | Chain sync application event
      Application ApplicationTrace
    | -- | API server is starting
      ServeApi
    | -- | API documentation server is starting
      ServeDocs
    | -- | Mithril bootstrap event
      Mithril ImportTrace
    | -- | Exiting after bootstrap-only mode
      BootstrapOnlyExit Point
    | -- | HTTP service encountered an error
      HTTPServiceError String
    | -- | Application is connecting to the node
      ApplicationStarting

-- | Render a 'MainTraces' value to a human-readable log string.
renderMainTraces :: MainTraces -> String
renderMainTraces Boot = "Starting up Cardano UTxO CSMT client..."
renderMainTraces (NotEmpty point) =
    "Database is not empty, skipping initial setup. "
        ++ "Current base checkpoint at point: "
        ++ show point
renderMainTraces (New a) =
    "Database is empty, performing initial setup."
        ++ renderArmageddonTrace a
renderMainTraces (Update ut) =
    "Database update: " ++ renderUpdateTrace ut
renderMainTraces (Application at) =
    "Application event: " ++ renderApplicationTrace at
renderMainTraces ServeApi =
    "Starting API server..."
renderMainTraces ServeDocs =
    "Starting API documentation server..."
renderMainTraces (Mithril mt) =
    "Mithril: " ++ renderImportTrace mt
renderMainTraces (BootstrapOnlyExit point) =
    "Bootstrap complete, exiting (--mithril-bootstrap-only). Checkpoint: "
        ++ show point
renderMainTraces (HTTPServiceError err) =
    "ERROR: HTTP service failed to start: " ++ err
renderMainTraces ApplicationStarting =
    "Starting Ouroboros node connection and chain sync application..."

{- | Extract metrics events from main traces for interception.

This function is used with the trace interceptor to forward relevant
events to the metrics system without modifying the trace pipeline.
Returns 'Just' for traces that should trigger metrics updates,
'Nothing' otherwise.
-}
stealMetricsEvent
    :: MainTraces
    -- ^ The trace to inspect
    -> Maybe MetricsEvent
    -- ^ Corresponding metrics event, if any
stealMetricsEvent (Update (UpdateForwardTip _ _ _ (Just merkleRoot))) =
    Just $ MerkleRootEvent merkleRoot
stealMetricsEvent (NotEmpty point) =
    Just $ BaseCheckpointEvent point
stealMetricsEvent (Mithril ImportStarting) =
    Just $ BootstrapPhaseEvent Downloading
stealMetricsEvent (Mithril (ImportExtractingUTxO _)) =
    Just $ BootstrapPhaseEvent Extracting
stealMetricsEvent (Mithril (ImportExtraction (ExtractionCounting count))) =
    Just $ ExtractionProgressEvent count
stealMetricsEvent (Mithril (ImportExtraction (ExtractionDecodedState total))) =
    Just $ ExtractionTotalEvent total
stealMetricsEvent (Mithril (ImportExtraction ExtractionStreamStarting)) =
    Just $ BootstrapPhaseEvent Extracting
stealMetricsEvent (Mithril (ImportExtraction (ExtractionProgress count))) =
    Just $ ExtractionProgressEvent count
stealMetricsEvent _ = Nothing
