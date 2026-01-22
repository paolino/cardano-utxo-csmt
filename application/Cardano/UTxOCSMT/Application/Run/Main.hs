{-# LANGUAGE NumericUnderscores #-}

module Cardano.UTxOCSMT.Application.Run.Main
    ( main
    )
where

import CSMT (FromKV (..))
import CSMT.Hashes
    ( Hash (..)
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
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
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
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
    ( MetricsEvent (MerkleRootEvent)
    , MetricsParams (..)
    , metricsTracer
    )
import Cardano.UTxOCSMT.Application.Options
    ( Options (..)
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
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Point
    )
import Control.Lens (Prism', lazy, prism', strict, view)
import Control.Monad (unless, when)
import Control.Tracer
    ( Contravariant (..)
    , Tracer (..)
    )
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Short (fromShort)
import Data.ByteString.Short qualified as B
import Data.Maybe (isNothing)
import Data.Serialize
    ( getShortByteString
    , getWord32be
    , getWord64be
    , putShortByteString
    , putWord32be
    , putWord64be
    )
import Data.Serialize.Extra (evalGetM, evalPutM)
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
import Database.KV.Cursor (firstEntry)
import Database.KV.Transaction (iterating)
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (..)
    , DB
    , withDBCF
    )
import OptEnvConf (runParser)
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Point qualified as Network
import Paths_cardano_utxo_csmt (version)

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
    | NotEmpty
    | New ArmageddonTrace
    | Update (UpdateTrace Point Hash)
    | Application ApplicationTrace

renderMainTraces :: MainTraces -> String
renderMainTraces Boot = "Starting up Cardano UTxO CSMT client..."
renderMainTraces NotEmpty =
    "Database is not empty, skipping initial setup."
renderMainTraces (New a) =
    "Database is empty, performing initial setup."
        ++ renderArmageddonTrace a
renderMainTraces (Update ut) =
    "Database update: " ++ renderUpdateTrace ut
renderMainTraces (Application at) =
    "Application event: " ++ renderApplicationTrace at

main :: IO ()
main = do
    options@Options{dbPath, logPath} <-
        runParser
            version
            "Tracking cardano UTxOs in a CSMT in a rocksDB database"
            optionsParser
    logTracer logPath $ \basicTracer -> do
        metricsEvent <-
            metricsTracer
                $ MetricsParams
                    { qlWindow = 100
                    , utxoSpeedWindow = 1000
                    , blockSpeedWindow = 100
                    , metricsOutput = renderMetrics
                    , metricsFrequency = 1_000_000
                    }
        TraceWith{tracer, trace, contra} <-
            fmap (intercept metricsEvent getMerkleRoot)
                $ newThreadSafeTracer
                $ contramap renderMainTraces
                $ addTimestampsTracer basicTracer
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
            setupDB tracer runner
            _ <-
                application
                    options
                    metricsEvent
                    (contra Application)
                    state
                    slots
                    (mFinality runner)
            error "main: application exited unexpectedly"

getMerkleRoot :: MainTraces -> Maybe MetricsEvent
getMerkleRoot (Update (UpdateForwardTip _ _ _ (Just merkleRoot))) =
    Just $ MerkleRootEvent merkleRoot
getMerkleRoot _ = Nothing

setupDB
    :: Tracer IO MainTraces
    -> RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> IO ()
setupDB TraceWith{trace, contra} runner = do
    new <- checkEmptyRollbacks runner
    unless new $ trace NotEmpty
    when new $ setup (contra New) runner armageddonParams

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
