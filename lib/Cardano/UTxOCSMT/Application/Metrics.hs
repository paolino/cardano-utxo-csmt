{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Cardano.UTxOCSMT.Application.Metrics
Description : Real-time metrics collection for chain synchronization

This module provides a metrics collection system that tracks:

* Block fetch queue statistics
* UTxO change processing speed
* Block processing speed
* Current blockchain era
* Current Merkle root
* Chain tip slot (for sync status detection)

Metrics are collected via a 'Tracer' and can be output at configurable intervals.
-}
module Cardano.UTxOCSMT.Application.Metrics
    ( metricsTracer
    , MetricsEvent (..)
    , MetricsParams (..)
    , Metrics (..)
    , BootstrapPhase (..)
    , ExtractionProgress (..)
    , HeaderSyncProgress (..)
    , renderBlockPoint
    , renderPoint
    )
where

import CSMT.Hashes (Hash)
import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength (..)
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Header, Point)
import Control.Comonad (Comonad (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , flushTQueue
    , newTQueueIO
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTQueue
    , writeTVar
    )
import Control.Foldl (Fold (..), handles)
import Control.Foldl qualified as Fold
import Control.Lens
    ( APrism'
    , Lens'
    , aside
    , lens
    , to
    , (&)
    , (.~)
    , (?~)
    , _Wrapped
    )
import Control.Lens.TH (makePrisms)
import Control.Monad (forever, (<=<))
import Control.Tracer (Tracer (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Profunctor (Profunctor (..))
import Data.Proxy (Proxy (..))
import Data.SOP.Strict (index_NS)
import Data.Swagger
    ( ToSchema (..)
    , declareSchemaRef
    , description
    , properties
    , required
    )
import Data.Swagger qualified as Swagger
import Data.Text qualified as Text
import Data.Time (UTCTime, diffUTCTime)
import Data.Tracer.Timestamp (Timestamped (..), timestampTracer)
import Data.Word (Word64)
import GHC.IsList (IsList (..))
import Ouroboros.Consensus.HardFork.Combinator (OneEraHeader (..))
import Ouroboros.Consensus.HardFork.Combinator qualified as HF
import Ouroboros.Network.Block (SlotNo (..), blockPoint)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (Block (..), WithOrigin (..))

----- library functions to help with metrics collection -----

-- Track the speed of events over a sliding window.
speedoMeter :: Int -> Fold UTCTime Double
speedoMeter window = Fold count Nothing getSpeed
  where
    getSpeed Nothing = 0
    getSpeed (Just (Nothing, _, _)) = 0
    getSpeed (Just (Just (startTime, endTime, cnt), _, _)) =
        fromIntegral cnt / realToFrac (diffUTCTime endTime startTime)
    count acc time = case acc of
        Nothing -> Just (Nothing, time, 0)
        Just (speed, startTime, cnt)
            | cnt < window ->
                Just (speed, startTime, cnt + 1)
            | otherwise -> Just (Just (startTime, time, cnt), time, 0)

-- Average over a rolling window
averageOverWindow :: Fractional a => Int -> Fold a a
averageOverWindow window = Fold step [] getAverage
  where
    step xs x = take window (x : xs)
    getAverage xs =
        let l = length xs
        in  if l == 0 then 0 else sum xs / fromIntegral l

-- | Lens for accessing the event in a 'Timestamped' wrapper
timestampedEventL :: Lens' (Timestamped a) a
timestampedEventL = lens timestampedEvent (\t e -> t{timestampedEvent = e})

-- | Support to use event prisms with 'aside'
timedAsTuple :: Timestamped a -> (UTCTime, a)
timedAsTuple (Timestamped t e) = (t, e)

---------- Metrics specific code ----------

-- | Bootstrap phase during startup
data BootstrapPhase
    = -- | Downloading Mithril snapshot
      Downloading
    | -- | Counting UTxOs in snapshot
      Counting
    | -- | Extracting UTxOs from Mithril snapshot
      Extracting
    | -- | Syncing headers after Mithril import
      SyncingHeaders
    | -- | Fully synced with chain tip
      Synced
    deriving (Show, Eq)

instance ToJSON BootstrapPhase where
    toJSON = \case
        Downloading -> "downloading"
        Counting -> "counting"
        Extracting -> "extracting"
        SyncingHeaders -> "syncing_headers"
        Synced -> "synced"

instance ToSchema BootstrapPhase where
    declareNamedSchema _ =
        return
            $ Swagger.NamedSchema (Just "BootstrapPhase")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerString
            & Swagger.enum_
                ?~ [ "downloading"
                   , "counting"
                   , "extracting"
                   , "syncing_headers"
                   , "synced"
                   ]
            & description
                ?~ "Current bootstrap phase: extracting, syncing_headers, \
                   \or synced"

-- | The signal we receive to update the metrics
data MetricsEvent
    = -- | some blocks are going to be fetched
      BlockFetchEvent EventQueueLength
    | -- | one utxo change has been processed
      UTxOChangeEvent
    | -- | one block has been processed
      BlockInfoEvent Header
    | -- | the current merkle root
      MerkleRootEvent Hash
    | -- | the base checkpoint for the UTxO set
      BaseCheckpointEvent Point
    | -- | the current chain tip slot from ChainSync
      ChainTipEvent SlotNo
    | -- | current bootstrap phase
      BootstrapPhaseEvent BootstrapPhase
    | -- | extraction total (from decoded ledger state)
      ExtractionTotalEvent Word64
    | -- | extraction progress (UTxOs extracted so far)
      ExtractionProgressEvent Word64
    | -- | header sync progress (current slot, target slot)
      HeaderSyncProgressEvent SlotNo SlotNo
    | -- | download progress (bytes downloaded so far)
      DownloadProgressEvent Word64
    deriving (Show)

makePrisms ''MetricsEvent

-- | Progress of UTxO extraction from Mithril snapshot
data ExtractionProgress = ExtractionProgress
    { extractionCurrent :: Word64
    -- ^ Number of UTxOs extracted so far
    , extractionTotal :: Maybe Word64
    -- ^ Total number of UTxOs to extract (if known)
    , extractionPercent :: Maybe Double
    -- ^ Percentage complete (if total is known)
    , extractionRate :: Double
    -- ^ Extraction rate (UTxOs per second)
    }
    deriving (Show, Eq)

instance ToJSON ExtractionProgress where
    toJSON
        ExtractionProgress
            { extractionCurrent
            , extractionTotal
            , extractionPercent
            , extractionRate
            } =
            object
                [ "current" .= extractionCurrent
                , "total" .= extractionTotal
                , "percent" .= extractionPercent
                , "rate" .= extractionRate
                ]

instance ToSchema ExtractionProgress where
    declareNamedSchema _ = do
        word64Schema <- declareSchemaRef (Proxy @Word64)
        maybeWord64Schema <- declareSchemaRef (Proxy @(Maybe Word64))
        maybeDoubleSchema <- declareSchemaRef (Proxy @(Maybe Double))
        doubleSchema <- declareSchemaRef (Proxy @Double)
        return
            $ Swagger.NamedSchema (Just "ExtractionProgress")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("current", word64Schema)
                    , ("total", maybeWord64Schema)
                    , ("percent", maybeDoubleSchema)
                    , ("rate", doubleSchema)
                    ]
            & required .~ ["current", "rate"]
            & description
                ?~ "Progress of UTxO extraction from Mithril snapshot"

-- | Progress of header synchronization after Mithril import
data HeaderSyncProgress = HeaderSyncProgress
    { headerCurrentSlot :: SlotNo
    -- ^ Current slot being processed
    , headerTargetSlot :: SlotNo
    -- ^ Target slot to reach
    }
    deriving (Show, Eq)

instance ToJSON HeaderSyncProgress where
    toJSON HeaderSyncProgress{headerCurrentSlot, headerTargetSlot} =
        object
            [ "currentSlot" .= unSlotNo headerCurrentSlot
            , "targetSlot" .= unSlotNo headerTargetSlot
            ]

instance ToSchema HeaderSyncProgress where
    declareNamedSchema _ = do
        word64Schema <- declareSchemaRef (Proxy @Word64)
        return
            $ Swagger.NamedSchema (Just "HeaderSyncProgress")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("currentSlot", word64Schema)
                    , ("targetSlot", word64Schema)
                    ]
            & required .~ ["currentSlot", "targetSlot"]
            & description
                ?~ "Progress of header synchronization after Mithril import"

type TimestampedMetrics = Timestamped MetricsEvent

-- track the last block point seen
lastBlockPointFold
    :: Fold TimestampedMetrics (Maybe (UTCTime, Header))
lastBlockPointFold =
    lmap timedAsTuple
        $ handles (aside _BlockInfoEvent) Fold.last

-- track average block fetch queue length
averageBlockFetchLength :: Int -> Fold TimestampedMetrics Double
averageBlockFetchLength window =
    handles
        (timestampedEventL . _BlockFetchEvent . _Wrapped . to fromIntegral)
        $ averageOverWindow window

-- track max block fetch queue length
maxBlockFetchQueueLength :: Fold TimestampedMetrics (Maybe Int)
maxBlockFetchQueueLength =
    handles
        (timestampedEventL . _BlockFetchEvent . _Wrapped)
        Fold.maximum

-- track speed of some event type
speedOfSomeEvent :: Int -> APrism' a b -> Fold (Timestamped a) Double
speedOfSomeEvent window prism =
    lmap timedAsTuple
        $ handles (aside prism)
        $ lmap fst
        $ speedoMeter window

-- speed of utxo changes processed
utxoSpeedFold :: Int -> Fold TimestampedMetrics Double
utxoSpeedFold window = speedOfSomeEvent window _UTxOChangeEvent

-- speed of blocks processed
blockSpeedFold :: Int -> Fold TimestampedMetrics Double
blockSpeedFold window = speedOfSomeEvent window _BlockInfoEvent

-- total number of utxo changes processed
totalUtxoChangesFold :: Fold TimestampedMetrics Int
totalUtxoChangesFold =
    handles
        (timestampedEventL . _UTxOChangeEvent)
        Fold.genericLength

currentEraFold :: Fold TimestampedMetrics (Maybe String)
currentEraFold =
    handles (timestampedEventL . _BlockInfoEvent) $ lmap getEra Fold.last
  where
    getEra :: Header -> String
    getEra (HF.HardForkHeader (OneEraHeader x)) = case index_NS x of
        0 -> "byron"
        1 -> "shelley"
        2 -> "allegra"
        3 -> "mary"
        4 -> "alonzo"
        5 -> "babbage"
        6 -> "conway"
        _ -> "unknown"

getCurrentMerkleRoot :: Fold TimestampedMetrics (Maybe Hash)
getCurrentMerkleRoot = handles (timestampedEventL . _MerkleRootEvent) Fold.last

getBaseCheckpoint :: Fold TimestampedMetrics (Maybe Point)
getBaseCheckpoint = handles (timestampedEventL . _BaseCheckpointEvent) Fold.last

-- track the chain tip slot
chainTipSlotFold :: Fold TimestampedMetrics (Maybe SlotNo)
chainTipSlotFold = handles (timestampedEventL . _ChainTipEvent) Fold.last

-- track bootstrap phase
bootstrapPhaseFold :: Fold TimestampedMetrics (Maybe BootstrapPhase)
bootstrapPhaseFold = handles (timestampedEventL . _BootstrapPhaseEvent) Fold.last

-- track extraction progress with rate calculation
extractionProgressFold
    :: Int -> Fold TimestampedMetrics (Maybe ExtractionProgress)
extractionProgressFold window =
    combine
        <$> handles (timestampedEventL . _ExtractionTotalEvent) Fold.last
        <*> handles (timestampedEventL . _ExtractionProgressEvent) Fold.last
        <*> speedOfSomeEvent window _ExtractionProgressEvent
  where
    combine _ Nothing _ = Nothing
    combine mTotal (Just current) rate =
        Just
            ExtractionProgress
                { extractionCurrent = current
                , extractionTotal = mTotal
                , extractionPercent = calcPercent current <$> mTotal
                , extractionRate = rate
                }
    calcPercent current total =
        (fromIntegral current / fromIntegral total) * 100

-- track header sync progress
headerSyncProgressFold
    :: Fold TimestampedMetrics (Maybe HeaderSyncProgress)
headerSyncProgressFold =
    handles (timestampedEventL . _HeaderSyncProgressEvent)
        $ lmap toProgress Fold.last
  where
    toProgress (current, target) =
        HeaderSyncProgress
            { headerCurrentSlot = current
            , headerTargetSlot = target
            }

-- track download progress (bytes downloaded)
downloadProgressFold :: Fold TimestampedMetrics (Maybe Word64)
downloadProgressFold = handles (timestampedEventL . _DownloadProgressEvent) Fold.last

-- | Tracked metrics
data Metrics = Metrics
    { averageQueueLength :: Double
    , maxQueueLength :: Maybe Int
    , utxoChangesCount :: Int
    , lastBlockPoint :: Maybe (UTCTime, Header)
    , utxoSpeed :: Double
    , blockSpeed :: Double
    , currentEra :: Maybe String
    , currentMerkleRoot :: Maybe Hash
    , baseCheckpoint :: Maybe Point
    , chainTipSlot :: Maybe SlotNo
    -- ^ The current chain tip slot from ChainSync protocol
    , bootstrapPhase :: Maybe BootstrapPhase
    -- ^ Current bootstrap phase during startup
    , extractionProgress :: Maybe ExtractionProgress
    -- ^ Progress of UTxO extraction from Mithril snapshot
    , headerSyncProgress :: Maybe HeaderSyncProgress
    -- ^ Progress of header synchronization after Mithril import
    , downloadedBytes :: Maybe Word64
    -- ^ Bytes downloaded during Mithril snapshot download
    }
    deriving (Show)

instance ToJSON Metrics where
    toJSON
        Metrics
            { averageQueueLength
            , maxQueueLength
            , utxoChangesCount
            , lastBlockPoint
            , utxoSpeed
            , blockSpeed
            , currentEra
            , currentMerkleRoot
            , baseCheckpoint
            , chainTipSlot
            , bootstrapPhase
            , extractionProgress
            , headerSyncProgress
            , downloadedBytes
            } =
            object
                [ "averageQueueLength" .= averageQueueLength
                , "maxQueueLength" .= maxQueueLength
                , "utxoChangesCount" .= utxoChangesCount
                , "lastBlockPoint"
                    .= fmap (Text.pack . renderBlockPoint) lastBlockPoint
                , "utxoSpeed" .= utxoSpeed
                , "blockSpeed" .= blockSpeed
                , "currentEra" .= currentEra
                , "currentMerkleRoot" .= fmap (Text.pack . show) currentMerkleRoot
                , "baseCheckpoint" .= fmap (Text.pack . renderPoint) baseCheckpoint
                , "chainTipSlot" .= fmap unSlotNo chainTipSlot
                , "bootstrapPhase" .= bootstrapPhase
                , "extractionProgress" .= extractionProgress
                , "headerSyncProgress" .= headerSyncProgress
                , "downloadedBytes" .= downloadedBytes
                ]

renderBlockPoint :: (a, Header) -> [Char]
renderBlockPoint (_, header) = renderPoint $ blockPoint header

-- | Render a Point as a string for display
renderPoint
    :: Point -> [Char]
renderPoint (Network.Point Origin) = "Origin"
renderPoint (Network.Point (At block)) =
    show (blockPointHash block)
        ++ "@"
        ++ show (unSlotNo $ blockPointSlot block)

instance ToSchema Metrics where
    declareNamedSchema _ = do
        doubleSchema <- declareSchemaRef (Proxy @Double)
        maybeIntSchema <- declareSchemaRef (Proxy @(Maybe Int))
        intSchema <- declareSchemaRef (Proxy @Int)
        maybeStringSchema <- declareSchemaRef (Proxy @(Maybe String))
        maybeWord64Schema <- declareSchemaRef (Proxy @(Maybe Word64))
        maybeBootstrapPhaseSchema <-
            declareSchemaRef (Proxy @(Maybe BootstrapPhase))
        maybeExtractionProgressSchema <-
            declareSchemaRef (Proxy @(Maybe ExtractionProgress))
        maybeHeaderSyncProgressSchema <-
            declareSchemaRef (Proxy @(Maybe HeaderSyncProgress))
        return
            $ Swagger.NamedSchema (Just "Metrics")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("averageQueueLength", doubleSchema)
                    , ("maxQueueLength", maybeIntSchema)
                    , ("utxoChangesCount", intSchema)
                    , ("lastBlockPoint", maybeStringSchema)
                    , ("utxoSpeed", doubleSchema)
                    , ("blockSpeed", doubleSchema)
                    , ("currentEra", maybeStringSchema)
                    , ("currentMerkleRoot", maybeStringSchema)
                    , ("baseCheckpoint", maybeStringSchema)
                    , ("chainTipSlot", maybeWord64Schema)
                    , ("bootstrapPhase", maybeBootstrapPhaseSchema)
                    , ("extractionProgress", maybeExtractionProgressSchema)
                    , ("headerSyncProgress", maybeHeaderSyncProgressSchema)
                    , ("downloadedBytes", maybeWord64Schema)
                    ]
            & required
                .~ [ "averageQueueLength"
                   , "maxQueueLength"
                   , "utxoChangesCount"
                   , "lastBlockPoint"
                   , "utxoSpeed"
                   , "blockSpeed"
                   , "currentEra"
                   , "currentMerkleRoot"
                   , "baseCheckpoint"
                   , "chainTipSlot"
                   , "bootstrapPhase"
                   , "extractionProgress"
                   , "headerSyncProgress"
                   , "downloadedBytes"
                   ]
            & description
                ?~ "Metrics about CSMT operations and blockchain synchronization"

-- | Metrics configuration parameters
data MetricsParams = MetricsParams
    { qlWindow :: Int
    -- ^ how many samples to consider for average queue length
    , utxoSpeedWindow :: Int
    -- ^ how many samples to consider for speed calculation
    , blockSpeedWindow :: Int
    -- ^ how many samples to consider for speed calculation
    , metricsOutput :: Metrics -> IO ()
    -- ^ function to output the metrics
    , metricsFrequency :: Int
    -- ^ frequency in microseconds to output the metrics
    }

-- track the whole set of metrics
metricsFold :: MetricsParams -> Fold TimestampedMetrics Metrics
metricsFold MetricsParams{qlWindow, utxoSpeedWindow, blockSpeedWindow} =
    Metrics
        <$> averageBlockFetchLength qlWindow
        <*> maxBlockFetchQueueLength
        <*> totalUtxoChangesFold
        <*> lastBlockPointFold
        <*> utxoSpeedFold utxoSpeedWindow
        <*> blockSpeedFold blockSpeedWindow
        <*> currentEraFold
        <*> getCurrentMerkleRoot
        <*> getBaseCheckpoint
        <*> chainTipSlotFold
        <*> bootstrapPhaseFold
        <*> extractionProgressFold utxoSpeedWindow
        <*> headerSyncProgressFold
        <*> downloadProgressFold

-- | Create a metrics tracer that collects metrics and outputs them
metricsTracer :: MetricsParams -> IO (Tracer IO MetricsEvent)
metricsTracer params@MetricsParams{metricsFrequency, metricsOutput} = do
    eventsQ <- newTQueueIO -- unbounded or we risk to slow down the application
    -- shared state for metrics accumulation
    metricsV <- newTVarIO $ metricsFold params
    link <=< async $ forever $ do
        -- let events accumulate, no need to load CPU as they come with timestamps
        threadDelay 100_000
        (es, currentFold) <- atomically $ do
            es <- flushTQueue eventsQ
            currentFold <- readTVar metricsV
            pure (es, currentFold)

        let !newFold = Fold.fold (duplicate currentFold) es
        atomically $ writeTVar metricsV newFold

    -- output loop
    link <=< async $ forever $ do
        threadDelay metricsFrequency
        readTVarIO metricsV >>= metricsOutput . extract
    pure
        $ timestampTracer
        $ Tracer
        $ \msg -> atomically $ writeTQueue eventsQ msg
