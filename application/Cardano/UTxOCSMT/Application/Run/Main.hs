{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.UTxOCSMT.Application.Run.Main
    ( main
    )
where

import CSMT (FromKV (..))
import CSMT.Hashes
    ( Hash (..)
    , fromKVHashes
    , generateInclusionProof
    , hashHashing
    , isoHash
    , mkHash
    , renderHash
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , ArmageddonTrace
    , renderArmageddonTrace
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( getAllMerkleRoots
    , getBaseCheckpoint
    , putBaseCheckpoint
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    , queryMerkleRoot
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( PartialHistory (..)
    , UpdateTrace (UpdateForwardTip)
    , newFinality
    , renderUpdateTrace
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRocksDBState
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( MetricsEvent (BaseCheckpointEvent, MerkleRootEvent)
    , MetricsParams (..)
    , metricsTracer
    )
import Cardano.UTxOCSMT.Application.Options
    ( MithrilOptions (..)
    , Options (..)
    , optionsParser
    )
import Cardano.UTxOCSMT.Application.Run.Application
    ( ApplicationTrace
    , application
    , renderApplicationTrace
    )
import Cardano.UTxOCSMT.Application.Run.RenderMetrics
    ( renderMetrics
    )
import Cardano.UTxOCSMT.Application.UTxOs (unsafeMkTxIn)
import Cardano.UTxOCSMT.HTTP.API
    ( InclusionProofResponse (..)
    , MerkleRootEntry (..)
    )
import Cardano.UTxOCSMT.HTTP.Base16
    ( encodeBase16Text
    , unsafeDecodeBase16Text
    )
import Cardano.UTxOCSMT.HTTP.Server (runAPIServer, runDocsServer)
import Cardano.UTxOCSMT.Mithril.Client (defaultMithrilConfig)
import Cardano.UTxOCSMT.Mithril.Client qualified as MithrilClient
import Cardano.UTxOCSMT.Mithril.Import
    ( ImportResult (..)
    , ImportTrace (..)
    , importFromMithril
    , renderImportTrace
    )
import Cardano.UTxOCSMT.Mithril.Options qualified as Mithril
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Point
    )
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , newTVarIO
    , readTVarIO
    , writeTVar
    )
import Control.Lens (Prism', lazy, prism', strict, view)
import Control.Monad (when, (<=<))
import Control.Tracer
    ( Contravariant (..)
    , Tracer (..)
    )
import Data.ByteArray ()
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
import Data.ByteString (StrictByteString, toStrict)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Short (fromShort, toShort)
import Data.ByteString.Short qualified as B
import Data.Maybe (fromMaybe, isNothing)
import Data.Serialize
    ( getShortByteString
    , getWord32be
    , getWord64be
    , putShortByteString
    , putWord32be
    , putWord64be
    )
import Data.Serialize.Extra (evalGetM, evalPutM)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Tracer.Intercept (intercept)
import Data.Tracer.LogFile (logTracer)
import Data.Tracer.ThreadSafe (newThreadSafeTracer)
import Data.Tracer.Timestamps (addTimestampsTracer)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Data.Word (Word16)
import Database.KV.Cursor (firstEntry)
import Database.KV.Transaction (iterating, query)
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (..)
    , DB
    , withDBCF
    )
import Main.Utf8 (withUtf8)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OptEnvConf (runParser)
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Consensus.Ledger.SupportsPeerSelection (PortNumber)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Point qualified as Network
import Paths_cardano_utxo_csmt (version)
import System.Exit (exitSuccess)
import System.IO.Temp (withSystemTempDirectory)

withRocksDB
    :: FilePath
    -> (DB -> IO b)
    -> IO b
withRocksDB path = do
    withDBCF
        path
        config
        [ ("kv", config)
        , ("csmt", config)
        , ("rollbacks", config)
        , ("config", config)
        ]

config :: Config
config =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

data MainTraces
    = Boot
    | NotEmpty Point
    | New ArmageddonTrace
    | Update (UpdateTrace Point Hash)
    | Application ApplicationTrace
    | ServeApi
    | ServeDocs
    | Mithril ImportTrace
    | BootstrapOnlyExit Point

renderMainTraces :: MainTraces -> String
renderMainTraces Boot = "Starting up Cardano UTxO CSMT client..."
renderMainTraces (NotEmpty point) =
    "Database is not empty, skipping initial setup. Current base checkpoint at point: "
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

startHTTPService
    :: IO () -> Maybe PortNumber -> (PortNumber -> IO ()) -> IO ()
startHTTPService _ Nothing _ = return ()
startHTTPService trace (Just port) runServer = do
    trace
    link <=< async $ runServer port

main :: IO ()
main = withUtf8 $ do
    options@Options
        { dbPath
        , logPath
        , apiPort
        , metricsOn
        , startingPoint
        , nodeName
        , portNumber
        , networkMagic
        , headersQueueSize
        , mithrilOptions
        } <-
        runParser
            version
            "Tracking cardano UTxOs in a CSMT in a rocksDB database"
            optionsParser
    logTracer logPath $ \basicTracer -> do
        metricsVar <- newTVarIO Nothing
        metricsEvent <-
            metricsTracer
                $ MetricsParams
                    { qlWindow = 100
                    , utxoSpeedWindow = 1000
                    , blockSpeedWindow = 100
                    , metricsOutput = \ !metrics -> do
                        when metricsOn $ renderMetrics metrics
                        atomically $ writeTVar metricsVar (Just metrics)
                    , metricsFrequency = 1_000_000
                    }
        TraceWith{tracer, trace, contra} <-
            fmap (intercept metricsEvent stealMetricsEvent)
                $ newThreadSafeTracer
                $ contramap renderMainTraces
                $ addTimestampsTracer basicTracer
        startHTTPService (trace ServeDocs) (apiDocsPort options)
            $ flip runDocsServer apiPort
        trace Boot
        withRocksDB dbPath $ \db -> do
            ((state, slots), runner) <-
                newRocksDBState
                    (contra Update)
                    db
                    prisms
                    context
                    Partial
                    slotHash
                    armageddonParams
            startHTTPService (trace ServeApi) apiPort
                $ \port ->
                    runAPIServer
                        port
                        (readTVarIO metricsVar)
                        (queryMerkleRoots runner)
                        (queryInclusionProof runner)
            startingPoint' <- setupDB tracer startingPoint mithrilOptions runner

            -- Check if we should exit after bootstrap
            when (Mithril.mithrilBootstrapOnly mithrilOptions) $ do
                trace $ BootstrapOnlyExit startingPoint'
                exitSuccess

            -- Emit the base checkpoint to metrics
            _ <-
                application
                    networkMagic
                    nodeName
                    portNumber
                    startingPoint'
                    headersQueueSize
                    metricsEvent
                    (contra Application)
                    state
                    slots
                    (mFinality runner)
            error "main: application exited unexpectedly"

stealMetricsEvent :: MainTraces -> Maybe MetricsEvent
stealMetricsEvent (Update (UpdateForwardTip _ _ _ (Just merkleRoot))) =
    Just $ MerkleRootEvent merkleRoot
stealMetricsEvent (NotEmpty point) =
    Just $ BaseCheckpointEvent point
stealMetricsEvent _ = Nothing

setupDB
    :: Tracer IO MainTraces
    -> Point
    -> MithrilOptions
    -> RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> IO Point
setupDB TraceWith{trace, contra} startingPoint mithrilOpts runner@RunCSMTTransaction{txRunTransaction} = do
    new <- checkEmptyRollbacks runner
    if new
        then do
            -- Check if Mithril bootstrap is enabled
            if mithrilEnabled mithrilOpts
                then bootstrapFromMithril
                else regularSetup
        else do
            response <- txRunTransaction getBaseCheckpoint
            case response of
                Nothing ->
                    error "setupDB: Database is not empty but no base checkpoint found"
                Just point -> do
                    trace $ NotEmpty point
                    return point
  where
    regularSetup = do
        setup (contra New) runner armageddonParams
        txRunTransaction $ putBaseCheckpoint startingPoint
        return startingPoint

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
            mithrilConfig =
                baseConfig
                    { MithrilClient.mithrilAggregatorUrl =
                        fromMaybe
                            (MithrilClient.mithrilAggregatorUrl baseConfig)
                            (Mithril.mithrilAggregatorUrl mithrilOpts)
                    , MithrilClient.mithrilClientPath =
                        Mithril.mithrilClientPath mithrilOpts
                    }

        result <-
            importFromMithril
                (contramap Mithril (contra id))
                mithrilConfig
                runner

        case result of
            ImportSuccess checkpoint _count _dbPath -> do
                -- Mithril import succeeded, UTxOs already imported
                setup (contra New) runner armageddonParams
                txRunTransaction $ putBaseCheckpoint checkpoint
                return checkpoint
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

checkEmptyRollbacks
    :: RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> IO Bool
checkEmptyRollbacks (RunCSMTTransaction runCSMT) =
    runCSMT $ do
        mfe <- iterating RollbackPoints firstEntry
        return $ isNothing mfe

mFinality
    :: (Ord key, MonadFail m)
    => RunCSMTTransaction cf op Point hash key value m
    -> m (Maybe Point)
mFinality (RunCSMTTransaction runCSMT) = runCSMT $ newFinality isFinal
  where
    isFinal :: WithOrigin Point -> WithOrigin Point -> Bool
    isFinal tip finality = distance tip finality > 2160

distance :: WithOrigin Point -> WithOrigin Point -> SlotNo
distance Origin _ = SlotNo 0
distance (At (Network.Point Origin)) _ = error "distance: tip at Origin has no slot"
distance (At (Network.Point (At (Network.Block slotTip _)))) Origin =
    slotTip
distance
    (At (Network.Point (At (Network.Block slotTip _))))
    (At (Network.Point (At (Network.Block slotFinality _)))) =
        SlotNo (unSlotNo slotTip - unSlotNo slotFinality)
distance _ _ = error "distance: finality at Origin has no slot"

armageddonParams :: ArmageddonParams Hash
armageddonParams =
    ArmageddonParams
        { noHash = mkHash ""
        , armageddonBatchSize = 1000
        }

context :: CSMTContext Hash LazyByteString LazyByteString
context =
    CSMTContext
        { fromKV =
            FromKV
                { fromK = fromK fromKVHashes . view strict
                , fromV = fromV fromKVHashes . view strict
                }
        , hashing = hashHashing
        }

prisms :: Prisms Point Hash LazyByteString LazyByteString
prisms = Prisms{..}
  where
    slotP :: Prism' StrictByteString Point
    slotP = prism' encode decode
      where
        encode :: Point -> StrictByteString
        encode (Network.Point Origin) = ""
        encode (Network.Point (At (Network.Block (SlotNo slot) (OneEraHash h)))) = do
            evalPutM $ do
                putWord64be slot
                putWord32be (fromIntegral $ B.length h)
                putShortByteString h

        decode :: StrictByteString -> Maybe Point
        decode bs
            | bs == "" = Just $ Network.Point Origin
            | otherwise = flip evalGetM bs $ do
                slot <- SlotNo <$> getWord64be
                len <- fromIntegral <$> getWord32be
                h <- getShortByteString len
                return $ Network.Point (At (Network.Block slot (OneEraHash h)))

    hashP :: Prism' StrictByteString Hash
    hashP = isoHash

    keyP :: Prism' StrictByteString LazyByteString
    keyP = lazy

    valueP :: Prism' StrictByteString LazyByteString
    valueP = lazy

slotHash :: Point -> Hash
slotHash (Network.Point Origin) = error "slotHash: Origin has no hash"
slotHash (Network.Point (At (Network.Block _ (OneEraHash h)))) =
    Hash $ fromShort h

-- | Query all merkle roots from the database
queryMerkleRoots
    :: RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> IO [MerkleRootEntry]
queryMerkleRoots (RunCSMTTransaction runCSMT) =
    runCSMT $ concatMap toMerkleRootEntry <$> getAllMerkleRoots
  where
    toMerkleRootEntry (slot, blockHash, merkleRoot) = case slot of
        Origin -> []
        At (Network.Point Origin) -> []
        At (Network.Point (At (Network.Block slotNo _))) ->
            [MerkleRootEntry{slotNo, blockHash, merkleRoot}]

{- | Retrieve the current inclusion proof and UTxO value for a given tx-in.
Returns 'Nothing' missing entries.
-}
queryInclusionProof
    :: RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> Text
    -> Word16
    -> IO (Maybe InclusionProofResponse)
queryInclusionProof (RunCSMTTransaction runCSMT) txIdText txIx = do
    runCSMT $ do
        proofBytes <- generateInclusionProof fromKVLazy CSMTCol txIn
        txOut <- query KVCol txIn
        merkle <- queryMerkleRoot
        pure $ do
            proof' <- proofBytes
            out <- txOut
            let merkleText = fmap (Text.decodeUtf8 . convertToBase Base16 . renderHash) merkle
            pure
                InclusionProofResponse
                    { proofTxId = txIdText
                    , proofTxIx = txIx
                    , proofTxOut = encodeBase16Text $ toStrict out
                    , proofBytes = Text.decodeUtf8 $ convertToBase Base16 proof'
                    , proofMerkleRoot = merkleText
                    }
  where
    fromKVLazy =
        FromKV
            { fromK = fromK fromKVHashes . view strict
            , fromV = fromV fromKVHashes . view strict
            }

    txIn = unsafeMkTxIn (toShort $ unsafeDecodeBase16Text txIdText) txIx
