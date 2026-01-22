{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.UTxOCSMT.Application.Metrics
    ( metricsTracer
    , MetricsEvent (..)
    , MetricsParams (..)
    , Metrics (..)
    )
where

import CSMT.Hashes (Hash)
import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength (..)
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Header)
import Control.Comonad (Comonad (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , flushTQueue
    , modifyTVar
    , newTQueueIO
    , newTVarIO
    , readTVarIO
    , writeTQueue
    )
import Control.Foldl (Fold (..), handles)
import Control.Foldl qualified as Fold
import Control.Lens
    ( APrism'
    , aside
    , to
    , _Wrapped
    )
import Control.Lens.TH (makeLensesFor, makePrisms)
import Control.Monad (forever, (<=<))
import Control.Tracer (Tracer (..))
import Data.Profunctor (Profunctor (..))
import Data.SOP.Strict (index_NS)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Ouroboros.Consensus.HardFork.Combinator (OneEraHeader (..))
import Ouroboros.Consensus.HardFork.Combinator qualified as HF

----- libray functions to help with metrics collection -----

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

-- Event tagged with the time it was recorded.
data Timed a = Timed
    { timedTimestamp :: UTCTime
    , timedEvent :: a
    }

makeLensesFor
    [ ("timedEvent", "timedEventL")
    ]
    ''Timed

-- support to use event prisms with 'aside'
timedAsTuple :: Timed a -> (UTCTime, a)
timedAsTuple (Timed t e) = (t, e)

-- A tracer that tags metrics events with the current time.
traceMetricsWithTime
    :: Tracer IO (Timed a) -> Tracer IO a
traceMetricsWithTime (Tracer tr) = Tracer $ \me -> do
    now <- getCurrentTime
    tr $ Timed{timedTimestamp = now, timedEvent = me}

---------- Metrics specific code ----------

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

makePrisms ''MetricsEvent

type TimedMetrics = Timed MetricsEvent

-- track the last block point seen
lastBlockPointFold
    :: Fold (Timed MetricsEvent) (Maybe (UTCTime, Header))
lastBlockPointFold =
    lmap timedAsTuple
        $ handles (aside _BlockInfoEvent) Fold.last

-- track average block fetch queue length
averageBlockFetchLength :: Int -> Fold TimedMetrics Double
averageBlockFetchLength window =
    handles
        (timedEventL . _BlockFetchEvent . _Wrapped . to fromIntegral)
        $ averageOverWindow window

-- track max block fetch queue length
maxBlockFetchQueueLength :: Fold TimedMetrics (Maybe Int)
maxBlockFetchQueueLength =
    handles
        (timedEventL . _BlockFetchEvent . _Wrapped)
        Fold.maximum

-- track speed of some event type
speedOfSomeEvent :: Int -> APrism' a b -> Fold (Timed a) Double
speedOfSomeEvent window prism =
    lmap timedAsTuple
        $ handles (aside prism)
        $ lmap fst
        $ speedoMeter window

-- speed of utxo changes processed
utxoSpeedFold :: Int -> Fold TimedMetrics Double
utxoSpeedFold window = speedOfSomeEvent window _UTxOChangeEvent

-- speed of blocks processed
blockSpeedFold :: Int -> Fold TimedMetrics Double
blockSpeedFold window = speedOfSomeEvent window _BlockInfoEvent

-- total number of utxo changes processed
totalUtxoChangesFold :: Fold TimedMetrics Int
totalUtxoChangesFold =
    handles
        (timedEventL . _UTxOChangeEvent)
        Fold.genericLength

currentEraFold :: Fold TimedMetrics (Maybe String)
currentEraFold =
    handles (timedEventL . _BlockInfoEvent) $ lmap getEra Fold.last
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

getCurrentMerkleRoot :: Fold TimedMetrics (Maybe Hash)
getCurrentMerkleRoot = handles (timedEventL . _MerkleRootEvent) Fold.last

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
    }

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
metricsFold :: MetricsParams -> Fold TimedMetrics Metrics
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

-- | Create a metrics tracer that collects metrics and outputs them
metricsTracer :: MetricsParams -> IO (Tracer IO MetricsEvent)
metricsTracer params@MetricsParams{metricsFrequency, metricsOutput} = do
    eventsQ <- newTQueueIO -- unbounded or we risk to slow down the application
    metricsV <- newTVarIO $ metricsFold params
    let tracer =
            traceMetricsWithTime
                $ Tracer
                $ \msg -> atomically $ writeTQueue eventsQ msg
    link <=< async $ forever $ do
        -- let events accumulate, no need to load CPU as they come with timestamps
        threadDelay 100_000
        atomically $ do
            es <- flushTQueue eventsQ
            modifyTVar metricsV $ \metrics -> Fold.fold (duplicate metrics) es
    link <=< async $ forever $ do
        threadDelay metricsFrequency
        readTVarIO metricsV >>= metricsOutput . extract
    pure tracer
