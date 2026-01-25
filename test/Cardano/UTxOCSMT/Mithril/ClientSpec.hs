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
    , fetchLatestSnapshot
    )
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
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
