{-# LANGUAGE NumericUnderscores #-}

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
    , cleanup
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( clearBootstrapInProgress
    , getBaseCheckpoint
    , getSkipSlot
    , isBootstrapInProgress
    , putBaseCheckpoint
    , setBootstrapInProgress
    , setSkipSlot
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Options (MithrilOptions (..))
import Cardano.UTxOCSMT.Application.Run.Config
    ( decodePoint
    , encodePoint
    )
import Cardano.UTxOCSMT.Application.Run.Traces
    ( MainTraces (..)
    , NodeValidationTrace (..)
    )
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
import Cardano.UTxOCSMT.Ouroboros.Connection
    ( NodeConnectionError (..)
    , validateNodeConnection
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Monad (when)
import Control.Tracer (Contravariant (..), Tracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Maybe (isNothing)
import Data.Text (unpack)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Data.Word (Word16, Word64)
import Database.KV.Cursor (firstEntry)
import Database.KV.Transaction (iterating)
import Database.RocksDB (BatchOp, ColumnFamily)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket (PortNumber)
import Ouroboros.Network.Magic (NetworkMagic)
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

When Mithril bootstrap is enabled, validates the node connection first
to fail fast if the node is unreachable (rather than after a long import).
-}
setupDB
    :: Tracer IO MainTraces
    -- ^ Tracer for logging setup events
    -> Point
    -- ^ Default starting point (used if not bootstrapping)
    -> MithrilOptions
    -- ^ Mithril configuration options
    -> NetworkMagic
    -- ^ Network magic for node connection
    -> String
    -- ^ Node hostname
    -> PortNumber
    -- ^ Node port
    -> Bool
    -- ^ Skip node validation
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
    networkMagic
    nodeName
    nodePort
    skipValidation
    armageddonParams
    runner@RunCSMTTransaction{txRunTransaction} = do
        -- Check for incomplete bootstrap and clean up if needed
        incomplete <- txRunTransaction $ isBootstrapInProgress decodePoint
        when incomplete $ do
            trace IncompleteBootstrapDetected
            cleanup (contra New) runner armageddonParams
            trace IncompleteBootstrapCleaned

        new <- checkEmptyRollbacks runner
        if new
            then do
                -- Check if Mithril bootstrap is enabled
                -- --mithril-bootstrap-only implies --mithril-bootstrap
                if mithrilEnabled mithrilOpts
                    || Mithril.mithrilBootstrapOnly mithrilOpts
                    then do
                        -- Validate node connection before expensive Mithril import
                        validationOk <- validateNode
                        if validationOk
                            then bootstrapFromMithril
                            else regularSetup
                    else regularSetup
            else do
                (response, skipSlot) <- txRunTransaction $ do
                    cp <- getBaseCheckpoint decodePoint
                    ss <- getSkipSlot decodePoint
                    pure (cp, ss)
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
                                , setupMithrilSlot = skipSlot
                                }
      where
        -- \| Validate node connection, returning True if OK or skipped
        validateNode :: IO Bool
        validateNode
            | skipValidation = pure True
            | otherwise = do
                let portNum = fromIntegral nodePort :: Word16
                trace
                    $ NodeValidation
                    $ ValidatingNodeConnection nodeName portNum
                -- 30 second timeout (in microseconds)
                result <-
                    validateNodeConnection
                        networkMagic
                        nodeName
                        nodePort
                        30_000_000
                case result of
                    Right () -> do
                        trace $ NodeValidation NodeValidationSuccess
                        pure True
                    Left err -> do
                        trace $ NodeValidation $ NodeValidationFailed err
                        error
                            $ "Node connection validation failed: "
                                ++ renderConnectionError err
                                ++ "\n\nCheck your --node-name and --node-port "
                                ++ "settings, or use --skip-node-validation "
                                ++ "to bypass this check."

        renderConnectionError :: NodeConnectionError -> String
        renderConnectionError (NodeResolutionFailed msg) =
            "Failed to resolve hostname: " ++ msg
        renderConnectionError (NodeConnectionFailed msg) =
            "Connection failed: " ++ msg
        renderConnectionError NodeConnectionTimeout =
            "Connection timed out after 30 seconds"

        regularSetup = do
            setup (contra New) runner armageddonParams
            txRunTransaction
                $ putBaseCheckpoint decodePoint encodePoint startingPoint
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

        runMithrilBootstrap manager downloadDir =
            case Mithril.mithrilAggregatorUrl mithrilOpts of
                Nothing -> do
                    -- No aggregator URL provided - fail with clear error
                    trace
                        $ Mithril
                        $ ImportError MithrilClient.MithrilMissingAggregatorUrl
                    regularSetup
                Just aggregatorUrl -> do
                    let baseConfig =
                            defaultMithrilConfig
                                manager
                                (Mithril.mithrilNetwork mithrilOpts)
                                aggregatorUrl
                                downloadDir
                        -- Determine ancillary verification key
                        ancillaryVk
                            | Mithril.mithrilSkipAncillaryVerification
                                mithrilOpts =
                                Nothing
                            | Just keyHex <-
                                Mithril.mithrilAncillaryVk mithrilOpts =
                                either
                                    (const Nothing)
                                    Just
                                    (parseVerificationKey keyHex)
                            | otherwise =
                                Nothing
                        mithrilConfig =
                            baseConfig
                                { MithrilClient.mithrilGenesisVk =
                                    unpack
                                        <$> Mithril.mithrilGenesisVk mithrilOpts
                                , MithrilClient.mithrilClientPath =
                                    Mithril.mithrilClientPath mithrilOpts
                                , MithrilClient.mithrilAncillaryVk = ancillaryVk
                                }

                    -- Set bootstrap-in-progress marker before streaming
                    txRunTransaction
                        $ setBootstrapInProgress decodePoint encodePoint

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
                            -- Save checkpoint and skip slot (will be cleared
                            -- when we reach the Mithril slot during chain sync)
                            -- Also clear the bootstrap-in-progress marker
                            txRunTransaction $ do
                                putBaseCheckpoint
                                    decodePoint
                                    encodePoint
                                    importCheckpoint
                                setSkipSlot decodePoint encodePoint importSlot
                                clearBootstrapInProgress decodePoint encodePoint
                            return
                                SetupResult
                                    { setupStartingPoint = importCheckpoint
                                    , setupMithrilSlot = Just importSlot
                                    }
                        ImportFailed err -> do
                            trace $ Mithril $ ImportError err
                            error
                                $ "Mithril bootstrap failed: "
                                    <> show err
                                    <> "\nCannot continue with incomplete bootstrap."
                        ImportExtractionFailed err -> do
                            trace $ Mithril $ ImportExtractionError err
                            error
                                $ "Mithril extraction failed: "
                                    <> show err
                                    <> "\nCannot continue with incomplete bootstrap."

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
