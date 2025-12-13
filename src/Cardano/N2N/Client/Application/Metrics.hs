{-# LANGUAGE NumericUnderscores #-}

module Cardano.N2N.Client.Application.Metrics
    ( metricsTracer
    , MetricsEvent (..)
    )
where

import Cardano.N2N.Client.Application.BlockFetch
    ( EventQueueLength (..)
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , modifyTVar
    , newTBQueueIO
    , newTVarIO
    , readTBQueue
    , readTVar
    , readTVarIO
    , writeTBQueue
    , writeTVar
    )
import Control.Monad ((<=<))
import Control.Tracer (Tracer (..))
import Data.Function (fix)
import Data.Word (Word32)
import System.Console.ANSI (hCursorUpLine)
import System.IO
    ( hPutStrLn
    , stderr
    )

data MetricsEvent
    = BlockFetchMetrics EventQueueLength
    | BlockHeightMetrics Word32

data Metrics = Metrics
    { averageQueueLength :: Double
    , maxQueueLength :: Int
    , totalBlocksFetched :: Int
    , blockSpeed :: Double
    }

data MetricsState = MetricsState
    { msQueueLengthWindow :: [Int]
    , msLastBlockCount :: Int
    }

updateMetrics
    :: Int
    -> MetricsState
    -> Metrics
    -> MetricsEvent
    -> (Metrics, MetricsState)
updateMetrics l ms metrics (BlockFetchMetrics (EventQueueLength len)) =
    let newWindow = take l (len : msQueueLengthWindow ms)
        avgLen =
            fromIntegral (sum newWindow) / fromIntegral (length newWindow)
        maxLen = max len (maxQueueLength metrics)
    in  ( metrics
            { averageQueueLength = avgLen
            , maxQueueLength = maxLen
            }
        , ms{msQueueLengthWindow = newWindow}
        )
updateMetrics _ ms metrics (BlockHeightMetrics _) =
    (metrics{totalBlocksFetched = totalBlocksFetched metrics + 1}, ms)

metricsTracer :: Int -> IO (Tracer IO MetricsEvent)
metricsTracer l = do
    queue <- newTBQueueIO 100

    metricsState <-
        newTVarIO
            $ MetricsState{msQueueLengthWindow = [], msLastBlockCount = 0}
    -- Initial metrics
    metricsVar <-
        newTVarIO
            $ Metrics
                { averageQueueLength = 0
                , maxQueueLength = 0
                , totalBlocksFetched = 0
                , blockSpeed = 0
                }
    let tracer = Tracer $ \msg -> atomically $ writeTBQueue queue msg
    link <=< async $ fix $ \loop -> do
        atomically $ do
            state <- readTVar metricsState
            msg <- readTBQueue queue
            metrics <- readTVar metricsVar
            let (newMetrics, newState) =
                    updateMetrics l state metrics msg
            writeTVar metricsVar newMetrics
            writeTVar metricsState newState
        loop
    link <=< async $ fix $ \reportLoop -> do
        threadDelay 10_000_000 -- 10 seconds
        metrics <- readTVarIO metricsVar
        state <- readTVarIO metricsState
        hPutStrLn stderr
            $ "Average Queue Length: "
                ++ show (averageQueueLength metrics)
                ++ ", Max Queue Length: "
                ++ show (maxQueueLength metrics)
                ++ ", Total Blocks Fetched: "
                ++ show (totalBlocksFetched metrics)
                ++ ", Block Speed (blocks/sec): "
                ++ show @Double
                    ( let blocksFetched = totalBlocksFetched metrics
                      in  fromIntegral
                            (blocksFetched - msLastBlockCount state)
                            / 10.0
                    )
        atomically $ do
            modifyTVar metricsState $ \s ->
                s{msLastBlockCount = totalBlocksFetched metrics}
        hCursorUpLine stderr 1
        reportLoop
    pure tracer
