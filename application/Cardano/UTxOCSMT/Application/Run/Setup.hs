module Cardano.UTxOCSMT.Application.Run.Setup
    ( SetupResult (..)
    , setupDB
    , checkEmptyRollbacks
    )
where

-- \|
-- Module      : Cardano.UTxOCSMT.Application.Run.Setup
-- Description : Database initialization and Mithril bootstrap
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module handles database setup, including detection of existing data,
-- initialization of new databases, and optional bootstrapping from Mithril
-- snapshots for faster initial sync.

import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( getBaseCheckpoint
    , putBaseCheckpoint
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Options (MithrilOptions (..))
import Cardano.UTxOCSMT.Application.Run.Traces (MainTraces (..))
import Cardano.UTxOCSMT.Mithril.AncillaryVerifier
    ( parseVerificationKey
    )
import Cardano.UTxOCSMT.Mithril.Client (defaultMithrilConfig)
import Cardano.UTxOCSMT.Mithril.Client qualified as MithrilClient
import Cardano.UTxOCSMT.Mithril.Import
    ( ImportResult (..)
    , ImportTrace (..)
    , importFromMithril
    )
import Cardano.UTxOCSMT.Mithril.Options qualified as Mithril
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Tracer (Contravariant (..), Tracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (unpack)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Data.Word (Word64)
import Database.KV.Cursor (firstEntry)
import Database.KV.Transaction (iterating)
import Database.RocksDB (BatchOp, ColumnFamily)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO.Temp (withSystemTempDirectory)

{- | Result of database setup containing the starting point for chain sync
and optional Mithril slot information.
-}
data SetupResult = SetupResult
    { setupStartingPoint :: Point
    -- ^ Starting point for chain sync (Origin or Mithril checkpoint)
    , setupMithrilSlot :: Maybe Word64
    -- ^ If bootstrapped from Mithril, the slot to skip headers until
    }

{- | Set up the database, potentially bootstrapping from Mithril.

This function handles three scenarios:

  1. Existing database: Returns the stored checkpoint
  2. New database with Mithril: Downloads and imports UTxO snapshot
  3. New database without Mithril: Initializes empty database

If Mithril bootstrap fails, falls back to regular setup.
-}
setupDB
    :: Tracer IO MainTraces
    -- ^ Tracer for logging setup events
    -> Point
    -- ^ Default starting point (used if not bootstrapping)
    -> MithrilOptions
    -- ^ Mithril configuration options
    -> ArmageddonParams hash
    -- ^ Parameters for database initialization
    -> RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        hash
        LazyByteString
        LazyByteString
        IO
    -- ^ Database transaction runner
    -> IO SetupResult
    -- ^ Setup result with starting point and optional Mithril slot
setupDB
    TraceWith{tracer, trace, contra}
    startingPoint
    mithrilOpts
    armageddonParams
    runner@RunCSMTTransaction{txRunTransaction} = do
        new <- checkEmptyRollbacks runner
        if new
            then do
                -- Check if Mithril bootstrap is enabled
                -- --mithril-bootstrap-only implies --mithril-bootstrap
                if mithrilEnabled mithrilOpts
                    || Mithril.mithrilBootstrapOnly mithrilOpts
                    then bootstrapFromMithril
                    else regularSetup
            else do
                response <- txRunTransaction getBaseCheckpoint
                case response of
                    Nothing ->
                        error
                            "setupDB: Database is not empty but \
                            \no base checkpoint found"
                    Just point -> do
                        trace $ NotEmpty point
                        return
                            SetupResult
                                { setupStartingPoint = point
                                , setupMithrilSlot = Nothing
                                }
      where
        regularSetup = do
            setup (contra New) runner armageddonParams
            txRunTransaction $ putBaseCheckpoint startingPoint
            return
                SetupResult
                    { setupStartingPoint = startingPoint
                    , setupMithrilSlot = Nothing
                    }

        bootstrapFromMithril = do
            -- Create HTTP manager for Mithril API calls
            manager <- newManager tlsManagerSettings

            -- Determine download directory
            let downloadDir = Mithril.mithrilDownloadDir mithrilOpts

            case downloadDir of
                Just dir -> runMithrilBootstrap manager dir
                Nothing ->
                    -- Use temporary directory for downloads
                    withSystemTempDirectory "mithril-snapshot" $ \tempDir ->
                        runMithrilBootstrap manager tempDir

        runMithrilBootstrap manager downloadDir = do
            let baseConfig =
                    defaultMithrilConfig
                        manager
                        (Mithril.mithrilNetwork mithrilOpts)
                        downloadDir
                -- Determine ancillary verification key
                ancillaryVk
                    | Mithril.mithrilSkipAncillaryVerification mithrilOpts =
                        Nothing
                    | Just keyHex <- Mithril.mithrilAncillaryVk mithrilOpts =
                        either
                            (const Nothing)
                            Just
                            (parseVerificationKey keyHex)
                    | otherwise =
                        Nothing
                mithrilConfig =
                    baseConfig
                        { MithrilClient.mithrilAggregatorUrl =
                            fromMaybe
                                (MithrilClient.mithrilAggregatorUrl baseConfig)
                                (Mithril.mithrilAggregatorUrl mithrilOpts)
                        , MithrilClient.mithrilGenesisVk =
                            unpack <$> Mithril.mithrilGenesisVk mithrilOpts
                        , MithrilClient.mithrilClientPath =
                            Mithril.mithrilClientPath mithrilOpts
                        , MithrilClient.mithrilAncillaryVk = ancillaryVk
                        }

            result <-
                importFromMithril
                    (contramap Mithril tracer)
                    mithrilConfig
                    runner

            case result of
                ImportSuccess{importCheckpoint, importSlot} -> do
                    -- Mithril import succeeded, UTxOs already imported
                    -- Don't save checkpoint yet - it will be saved when
                    -- we reach the target slot during chain sync
                    setup (contra New) runner armageddonParams
                    -- Save Origin as initial checkpoint (will be updated
                    -- when we reach the Mithril slot)
                    txRunTransaction $ putBaseCheckpoint importCheckpoint
                    return
                        SetupResult
                            { setupStartingPoint = importCheckpoint
                            , setupMithrilSlot = Just importSlot
                            }
                ImportFailed err -> do
                    -- Fall back to regular setup if Mithril fails
                    trace $ Mithril $ ImportError err
                    regularSetup
                ImportExtractionFailed err -> do
                    -- Fall back to regular setup if extraction fails
                    trace $ Mithril $ ImportExtractionError err
                    regularSetup
                ImportSkipped _reason -> do
                    -- Fall back to regular setup
                    regularSetup

{- | Check if the rollbacks column family is empty.

Returns 'True' if the database is new (no rollback points stored),
'False' if it contains existing data.
-}
checkEmptyRollbacks
    :: RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        hash
        LazyByteString
        LazyByteString
        IO
    -- ^ Database transaction runner
    -> IO Bool
    -- ^ 'True' if database is empty
checkEmptyRollbacks (RunCSMTTransaction runCSMT) =
    runCSMT $ do
        mfe <- iterating RollbackPoints firstEntry
        return $ isNothing mfe
