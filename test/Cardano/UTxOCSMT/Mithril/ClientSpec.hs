{- |
Module      : Cardano.UTxOCSMT.Mithril.ClientSpec
Description : E2E tests for Mithril client

Tests that verify the Mithril client can successfully connect to and
retrieve data from Mithril aggregators.
-}
module Cardano.UTxOCSMT.Mithril.ClientSpec
    ( spec
    )
where

import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilNetwork (..)
    , SnapshotMetadata (..)
    , defaultMithrilConfig
    , downloadSnapshot
    , fetchLatestSnapshot
    , snapshotDigest
    )
import Cardano.UTxOCSMT.Mithril.Extraction
    ( ExtractionError (..)
    , findLedgerStateFile
    )
import Data.List (isInfixOf)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (findExecutable)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , it
    , pendingWith
    , shouldSatisfy
    )

spec :: Spec
spec = describe "Mithril Client E2E" $ do
    describe "fetchLatestSnapshot" $ do
        it "can fetch snapshot metadata from preview aggregator"
            $ testFetchSnapshot MithrilPreview
        it "can fetch snapshot metadata from preprod aggregator"
            $ testFetchSnapshot MithrilPreprod
        it "can fetch snapshot metadata from mainnet aggregator"
            $ testFetchSnapshot MithrilMainnet

    describe "downloadSnapshot" $ do
        it "downloads preview snapshot with ledger state" $ do
            hasMithril <- findExecutable "mithril-client"
            case hasMithril of
                Nothing ->
                    pendingWith "mithril-client not found in PATH"
                Just _ -> do
                    manager <- newManager tlsManagerSettings
                    withSystemTempDirectory "mithril-test" $ \tmpDir -> do
                        let config =
                                defaultMithrilConfig
                                    manager
                                    MithrilPreview
                                    tmpDir
                        -- Fetch snapshot metadata
                        fetchResult <- fetchLatestSnapshot config
                        case fetchResult of
                            Left err ->
                                fail
                                    $ "Failed to fetch snapshot: "
                                    ++ show err
                            Right snapshot -> do
                                -- Download the snapshot
                                downloadResult <-
                                    downloadSnapshot
                                        config
                                        (snapshotDigest snapshot)
                                case downloadResult of
                                    Left err ->
                                        fail
                                            $ "Failed to download: "
                                            ++ show err
                                    Right dbPath -> do
                                        -- Check that ledger state exists
                                        ledgerResult <-
                                            findLedgerStateFile dbPath
                                        case ledgerResult of
                                            Left (LedgerStateNotFound _) ->
                                                fail
                                                    "Ledger state directory \
                                                    \not found"
                                            Left (NoLedgerStateFiles _) ->
                                                fail
                                                    "No ledger state files \
                                                    \found"
                                            Left err ->
                                                fail $ "Error: " ++ show err
                                            Right (filePath, slot) -> do
                                                slot
                                                    `shouldSatisfy` (> 0)
                                                filePath
                                                    `shouldSatisfy` ( \p ->
                                                        ".lstate"
                                                            `isInfixOf` p
                                                                      )

testFetchSnapshot :: MithrilNetwork -> IO ()
testFetchSnapshot network = do
    manager <- newManager tlsManagerSettings
    let config = defaultMithrilConfig manager network "/tmp"
    result <- fetchLatestSnapshot config
    case result of
        Left err -> fail $ "Failed to fetch snapshot: " ++ show err
        Right snapshot -> do
            snapshotBeaconSlot snapshot `shouldSatisfy` (> 0)
            snapshotBeaconEpoch snapshot `shouldSatisfy` (> 0)
