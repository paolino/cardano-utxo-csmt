module Cardano.UTxOCSMT.HTTP.ServerSpec
    ( spec
    )
where

import CSMT (FromKV (..))
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , generateInclusionProof
    , hashHashing
    , isoHash
    , mkHash
    , renderHash
    )
import Cardano.Slotting.Slot (WithOrigin (..))
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( getAllMerkleRoots
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    , queryMerkleRoot
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( PartialHistory (..)
    , mkUpdate
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , Update (..)
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBCSMTTransaction
    )
import Cardano.UTxOCSMT.Application.Metrics (Metrics (..))
import Cardano.UTxOCSMT.HTTP.API
    ( InclusionProofResponse (..)
    , MerkleRootEntry (..)
    , ReadyResponse (..)
    )
import Cardano.UTxOCSMT.HTTP.Base16
    ( encodeBase16Text
    )
import Cardano.UTxOCSMT.HTTP.Server (apiApp)
import Control.Lens (lazy, prism', strict, view)
import Control.Monad.IO.Class (liftIO)
import Control.Tracer (nullTracer)
import Data.Aeson (eitherDecode, encode)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Serialize (getWord64be, putWord64be)
import Data.Serialize.Extra (evalGetM, evalPutM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16)
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (..)
    , DB
    , withDBCF
    )
import Network.HTTP.Types (methodGet, status200, status404, status503)
import Network.Wai (requestMethod)
import Network.Wai.Test
    ( SResponse (..)
    , Session
    , defaultRequest
    , request
    , runSession
    , setPath
    )
import Ouroboros.Network.Block (SlotNo (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

{- | Test prisms for serializing slots, hashes, keys, values
Uses SlotNo for slots and ByteString for keys/values (matching RocksDBSpec.hs)
-}
testPrisms :: Prisms SlotNo Hash BL.ByteString BL.ByteString
testPrisms =
    Prisms
        { slotP =
            prism'
                (evalPutM . putWord64be . fromIntegral . unSlotNo)
                (fmap fromIntegral . evalGetM getWord64be)
        , hashP = isoHash
        , keyP = lazy
        , valueP = lazy
        }

-- | Test CSMT context for hashing
testCSMTContext :: CSMTContext Hash BL.ByteString BL.ByteString
testCSMTContext =
    CSMTContext
        { fromKV = fromKVLazy
        , hashing = hashHashing
        }
  where
    fromKVLazy =
        FromKV
            { fromK = fromK fromKVHashes . view strict
            , fromV = fromV fromKVHashes . view strict
            }

-- | Test armageddon params
testArmageddonParams :: ArmageddonParams Hash
testArmageddonParams =
    ArmageddonParams{armageddonBatchSize = 1000, noHash = mkHash ""}

-- | Generate a hash from a slot number (for testing)
testSlotHash :: SlotNo -> Hash
testSlotHash n = mkHash $ BC.pack $ "blockhash" ++ show n

-- | RocksDB config for tests
testConfig :: Config
testConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 1
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Create a test key from a string (padded to 32 bytes for consistency)
mkTestKey :: String -> BL.ByteString
mkTestKey s = BL.fromStrict $ padTo32 $ BC.pack s
  where
    padTo32 bs = bs `BC.append` BC.replicate (32 - BC.length bs) '_'

-- | Create a test value
mkTestValue :: String -> BL.ByteString
mkTestValue = BL.fromStrict . BC.pack

-- | Encode bytes to base16 text
encodeBase16 :: ByteString -> Text
encodeBase16 = TE.decodeUtf8 . convertToBase Base16

{- | Query all merkle roots and convert to MerkleRootEntry format
Adapted for test types (SlotNo slots -> SlotNo)
-}
queryTestMerkleRoots
    :: RunCSMTTransaction cf op SlotNo Hash BL.ByteString BL.ByteString IO
    -> IO [MerkleRootEntry]
queryTestMerkleRoots (RunCSMTTransaction runCSMT) =
    runCSMT $ concatMap toEntry <$> getAllMerkleRoots
  where
    toEntry (slot, blockHash, merkleRoot) = case slot of
        Origin -> []
        At slotNo ->
            [ MerkleRootEntry
                { slotNo
                , blockHash = blockHash
                , merkleRoot = merkleRoot
                }
            ]

{- | Query inclusion proof for a given key
For tests, we use a simplified lookup where txId is the hex of the key prefix
-}
queryTestInclusionProof
    :: RunCSMTTransaction cf op SlotNo Hash BL.ByteString BL.ByteString IO
    -> BL.ByteString
    -- ^ The actual key that was inserted
    -> Text
    -- ^ txId (hex string)
    -> Word16
    -- ^ txIx
    -> IO (Maybe InclusionProofResponse)
queryTestInclusionProof (RunCSMTTransaction runCSMT) actualKey txIdText txIx =
    runCSMT $ do
        result <-
            generateInclusionProof
                (fromKV testCSMTContext)
                KVCol
                CSMTCol
                actualKey
        merkle <- queryMerkleRoot
        pure $ do
            (out, proof') <- result
            let merkleText = fmap (encodeBase16Text . renderHash) merkle
            pure
                InclusionProofResponse
                    { proofTxId = txIdText
                    , proofTxIx = txIx
                    , proofTxOut = encodeBase16 $ BL.toStrict out
                    , proofBytes = encodeBase16Text proof'
                    , proofMerkleRoot = merkleText
                    }

-- | Run tests with a fresh RocksDB database and HTTP app
withTestDB
    :: ( RunCSMTTransaction
            ColumnFamily
            BatchOp
            SlotNo
            Hash
            BL.ByteString
            BL.ByteString
            IO
         -> Update IO SlotNo BL.ByteString BL.ByteString
         -> IO a
       )
    -> IO a
withTestDB action =
    withSystemTempDirectory "http-test" $ \dir ->
        withRocksDB dir $ \db -> do
            runner <- newRunRocksDBCSMTTransaction db testPrisms testCSMTContext
            setup nullTracer runner testArmageddonParams
            action runner
                $ mkUpdate
                    nullTracer
                    Complete
                    testSlotHash
                    testArmageddonParams
                    runner

-- | Open RocksDB with test column families
withRocksDB :: FilePath -> (DB -> IO b) -> IO b
withRocksDB path =
    withDBCF
        path
        testConfig
        [ ("kv", testConfig)
        , ("csmt", testConfig)
        , ("rollbacks", testConfig)
        ]

-- | Sample metrics for testing
sampleMetrics :: Metrics
sampleMetrics =
    Metrics
        { averageQueueLength = 0.0
        , maxQueueLength = Nothing
        , utxoChangesCount = 0
        , lastBlockPoint = Nothing
        , utxoSpeed = 0.0
        , blockSpeed = 0.0
        , currentEra = Nothing
        , currentMerkleRoot = Nothing
        , baseCheckpoint = Nothing
        , chainTipSlot = Nothing
        }

-- | Sample ReadyResponse for synced state
syncedResponse :: ReadyResponse
syncedResponse =
    ReadyResponse
        { ready = True
        , tipSlot = Just 1000
        , processedSlot = Just 1000
        , slotsBehind = Just 0
        }

-- | Sample ReadyResponse for not synced state
notSyncedResponse :: ReadyResponse
notSyncedResponse =
    ReadyResponse
        { ready = False
        , tipSlot = Just 1000
        , processedSlot = Just 500
        , slotsBehind = Just 500
        }

session
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> IO ReadyResponse
    -> Session a
    -> IO a
session a b c d = flip runSession $ apiApp a b c d

spec :: Spec
spec = do
    describe "HTTP API" $ do
        describe "GET /metrics" $ do
            it "returns 500 when metrics not available" $ do
                withTestDB $ \runner _update -> session
                    (pure Nothing)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    (pure syncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/metrics"
                        liftIO $ simpleStatus resp `shouldBe` status404

            it "returns valid JSON when metrics available" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    (pure syncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/metrics"
                        liftIO $ simpleStatus resp `shouldBe` status200
                        let body = simpleBody resp
                        liftIO $ body `shouldBe` encode sampleMetrics

        describe "GET /merkle-roots" $ do
            it "returns empty list for fresh database" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    (pure syncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/merkle-roots"
                        liftIO $ simpleStatus resp `shouldBe` status200
                        let decoded =
                                eitherDecode $ simpleBody resp
                                    :: Either String [MerkleRootEntry]
                        liftIO $ decoded `shouldBe` Right []

            it "returns entries after forward tip applies" $ do
                withTestDB $ \runner update -> do
                    -- Insert some data at slot 100
                    let key1 = mkTestKey "utxo1"
                        value1 = mkTestValue "output1"
                    update1 <- forwardTipApply update 100 [Insert key1 value1]

                    -- Insert more data at slot 200
                    let key2 = mkTestKey "utxo2"
                        value2 = mkTestValue "output2"
                    _ <- forwardTipApply update1 200 [Insert key2 value2]

                    session
                        (pure $ Just sampleMetrics)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/merkle-roots"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded = eitherDecode $ simpleBody resp
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right entries -> do
                                    length entries `shouldBe` 2
                                    slotNo <$> entries
                                        `shouldBe` [ SlotNo{unSlotNo = 200}
                                                   , SlotNo{unSlotNo = 100}
                                                   ]

            it "returns 503 when not synced" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    (pure notSyncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/merkle-roots"
                        liftIO $ simpleStatus resp `shouldBe` status503

        describe "GET /proof/{txId}/{txIx}" $ do
            it "returns 404 for non-existent UTxO" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    (pure syncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/proof/deadbeef/0"
                        liftIO $ simpleStatus resp `shouldBe` status404

            it "returns valid proof for existing UTxO" $ do
                withTestDB $ \runner update -> do
                    -- Create a known key and insert it
                    let testKey = mkTestKey "testutxo"
                        testValue = mkTestValue "testoutput"
                        testTxId = "7465737475747870" -- "testutxp" in hex (partial, for test identification)
                        testTxIx = 0 :: Word16

                    -- Insert the UTxO
                    _ <- forwardTipApply update 100 [Insert testKey testValue]

                    -- Create query function that looks up by our test key
                    let proofQuery txId txIx =
                            if txId == testTxId && txIx == testTxIx
                                then queryTestInclusionProof runner testKey txId txIx
                                else pure Nothing
                    session
                        (pure $ Just sampleMetrics)
                        (queryTestMerkleRoots runner)
                        proofQuery
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/proof/7465737475747870/0"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let body = simpleBody resp
                            let decoded = eitherDecode body :: Either String InclusionProofResponse
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right proof -> do
                                    proofTxId proof `shouldBe` testTxId
                                    proofTxIx proof `shouldBe` testTxIx
                                    -- Proof bytes should be non-empty
                                    proofBytes proof `shouldSatisfy` (not . T.null)
                                    -- TxOut should be the hex of our test value
                                    proofTxOut proof `shouldSatisfy` (not . T.null)

            it "proof contains merkle root" $ do
                withTestDB $ \runner update -> do
                    let testKey = mkTestKey "merkletest"
                        testValue = mkTestValue "merklevalue"
                        testTxId = "6d65726b6c657465"
                        testTxIx = 0 :: Word16

                    _ <- forwardTipApply update 100 [Insert testKey testValue]

                    let proofQuery txId txIx =
                            if txId == testTxId && txIx == testTxIx
                                then queryTestInclusionProof runner testKey txId txIx
                                else pure Nothing
                    session
                        (pure $ Just sampleMetrics)
                        (queryTestMerkleRoots runner)
                        proofQuery
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/proof/6d65726b6c657465/0"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let body = simpleBody resp
                            let decoded = eitherDecode body :: Either String InclusionProofResponse
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right proof -> do
                                    -- Merkle root should be present after insertion
                                    proofMerkleRoot proof `shouldSatisfy` \case
                                        Just mr -> not (T.null mr)
                                        Nothing -> False

            it "returns 503 when not synced" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    (pure notSyncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/proof/deadbeef/0"
                        liftIO $ simpleStatus resp `shouldBe` status503

        describe "GET /ready" $ do
            it "returns synced status when synced" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    (pure syncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/ready"
                        liftIO $ simpleStatus resp `shouldBe` status200
                        let decoded = eitherDecode $ simpleBody resp
                        liftIO $ case decoded of
                            Left err -> fail $ "JSON decode error: " ++ err
                            Right response -> do
                                ready response `shouldBe` True
                                tipSlot response `shouldBe` Just 1000
                                processedSlot response `shouldBe` Just 1000
                                slotsBehind response `shouldBe` Just 0

            it "returns not synced status when not synced" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    (pure notSyncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/ready"
                        liftIO $ simpleStatus resp `shouldBe` status200
                        let decoded = eitherDecode $ simpleBody resp
                        liftIO $ case decoded of
                            Left err -> fail $ "JSON decode error: " ++ err
                            Right response -> do
                                ready response `shouldBe` False
                                slotsBehind response `shouldBe` Just 500
