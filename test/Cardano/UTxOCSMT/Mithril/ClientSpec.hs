{-# LANGUAGE BangPatterns #-}

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

import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilNetwork (..)
    , SnapshotMetadata (..)
    , defaultMithrilConfig
    , downloadSnapshotHttp
    , fetchLatestSnapshot
    )
import Cardano.UTxOCSMT.Mithril.Extraction
    ( ExtractionError (..)
    , extractUTxOsFromSnapshot
    , findLedgerStateFile
    )
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.MemPack (unpack)
import Data.MemPack.Error (SomeError)
import Data.Word (Word64)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Streaming.Prelude qualified as S
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , it
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

    describe "downloadSnapshotHttp" $ do
        it "downloads preview snapshot and verifies ancillary" $ do
            testDownloadAndVerify MithrilPreview

        it "downloads preprod snapshot and verifies ancillary" $ do
            testDownloadAndVerify MithrilPreprod

-- | Test downloading and verifying ancillary files for a network
testDownloadAndVerify :: MithrilNetwork -> IO ()
testDownloadAndVerify network = do
    manager <- newManager tlsManagerSettings
    withSystemTempDirectory "mithril-test" $ \tmpDir -> do
        let config = defaultMithrilConfig manager network tmpDir
        -- Fetch snapshot metadata
        fetchResult <- fetchLatestSnapshot config
        case fetchResult of
            Left err ->
                fail $ "Failed to fetch snapshot: " ++ show err
            Right snapshot -> do
                -- Download via HTTP (with verification if key available)
                downloadResult <- downloadSnapshotHttp config snapshot
                case downloadResult of
                    Left err ->
                        fail $ "Failed to download: " ++ show err
                    Right dbPath -> do
                        -- Check that ledger state exists
                        ledgerResult <- findLedgerStateFile dbPath
                        case ledgerResult of
                            Left (LedgerStateNotFound _) ->
                                fail "Ledger state directory not found"
                            Left (NoLedgerStateFiles _) ->
                                fail "No ledger state files found"
                            Left err ->
                                fail $ "Error: " ++ show err
                            Right (filePath, slot) -> do
                                slot `shouldSatisfy` (> 0)
                                -- New format: ledger/<slot>/
                                -- Old format: ledger/<slot>-<hash>.lstate
                                filePath
                                    `shouldSatisfy` (\p -> "ledger" `isInfixOf` p)
                                -- Extract UTxOs and verify TxIn decoding
                                extractResult <-
                                    extractUTxOsFromSnapshot
                                        nullTracer
                                        dbPath
                                        (verifyTxInDecoding 100)
                                case extractResult of
                                    Left err ->
                                        fail
                                            $ "Extraction failed: "
                                                ++ show err
                                    Right ((count, decoded), _slot) -> do
                                        -- Should have UTxOs
                                        count `shouldSatisfy` (> 0)
                                        -- All sampled TxIns should
                                        -- decode successfully
                                        decoded `shouldSatisfy` (>= 100)

{- | Verify extracted UTxO bytes can be decoded as Conway-era TxIn/TxOut

The tvar file stores UTxOs as MemPack-encoded (TxIn, TxOut) pairs.
This test verifies both can be decoded using cardano-ledger types.
-}
verifyTxInDecoding
    :: Int
    -- ^ Number of samples to decode
    -> S.Stream (S.Of (LBS.ByteString, LBS.ByteString)) IO ()
    -> IO (Word64, Int)
    -- ^ (total count, successfully decoded samples)
verifyTxInDecoding samples = go 0 0
  where
    go !count !decoded stream = do
        result <- S.next stream
        case result of
            Left () -> pure (count, decoded)
            Right ((keyBs, valBs), rest) -> do
                let newCount = count + 1
                    -- Try to decode as TxIn using MemPack
                    txInResult :: Either SomeError TxIn
                    txInResult = unpack (LBS.toStrict keyBs)
                    -- Try to decode as TxOut using MemPack
                    txOutResult :: Either SomeError (BabbageTxOut ConwayEra)
                    txOutResult = unpack (LBS.toStrict valBs)
                    -- Count as decoded only if both succeed
                    newDecoded =
                        if newCount <= fromIntegral samples
                            then case (txInResult, txOutResult) of
                                (Right _, Right _) -> decoded + 1
                                _ -> decoded
                            else decoded
                go newCount newDecoded rest

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
