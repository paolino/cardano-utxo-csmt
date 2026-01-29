{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

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
import Cardano.UTxOCSMT.Application.Metrics.Types
    ( BootstrapPhase (..)
    , ExtractionProgress (..)
    , HeaderSyncProgress (..)
    , Metrics (..)
    , MetricsEvent (..)
    , MetricsParams (..)
    , renderBlockPoint
    , renderPoint
    , _BaseCheckpointEvent
    , _BlockFetchEvent
    , _BlockInfoEvent
    , _BootstrapPhaseEvent
    , _ChainTipEvent
    , _CountingProgressEvent
    , _DownloadProgressEvent
    , _ExtractionProgressEvent
    , _ExtractionTotalEvent
    , _HeaderSyncProgressEvent
    , _MerkleRootEvent
    , _UTxOChangeEvent
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
import Control.Foldl.Extra (averageOverWindow, speedoMeter)
import Control.Lens
    ( APrism'
    , Lens'
    , aside
    , lens
    , to
    , _Wrapped
    )
import Control.Monad (forever, (<=<))
import Control.Tracer (Tracer (..))
import Data.Profunctor (Profunctor (..))
import Data.SOP.Strict (index_NS)
import Data.Time (UTCTime)
import Data.Tracer.Timestamp (Timestamped (..), timestampTracer)
import Data.Word (Word64)
import Ouroboros.Consensus.HardFork.Combinator (OneEraHeader (..))
import Ouroboros.Consensus.HardFork.Combinator qualified as HF
import Ouroboros.Network.Block (SlotNo (..))

-- | Lens for accessing the event in a 'Timestamped' wrapper
timestampedEventL :: Lens' (Timestamped a) a
timestampedEventL = lens timestampedEvent (\t e -> t{timestampedEvent = e})

-- | Support to use event prisms with 'aside'
timedAsTuple :: Timestamped a -> (UTCTime, a)
timedAsTuple (Timestamped t e) = (t, e)

---------- Metrics specific folds ----------

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

-- track counting progress (UTxOs counted so far)
countingProgressFold :: Fold TimestampedMetrics (Maybe Word64)
countingProgressFold = handles (timestampedEventL . _CountingProgressEvent) Fold.last

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
        <*> countingProgressFold

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
