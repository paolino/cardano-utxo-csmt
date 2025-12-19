{-# LANGUAGE NumericUnderscores #-}

module Cardano.N2N.Client.Application.Metrics
    ( metricsTracer
    , MetricsEvent (..)
    )
where

import Cardano.N2N.Client.Application.BlockFetch
    ( EventQueueLength (..)
    )
import Cardano.N2N.Client.Ouroboros.Types (Header, Point)
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
import Ouroboros.Consensus.Block
    ( BlockNo (BlockNo)
    , SlotNo (unSlotNo)
    , blockNo
    , blockPoint
    )
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point
    ( Block (blockPointHash)
    , WithOrigin (..)
    , blockPointSlot
    )
import System.Console.ANSI (hClearScreen, hSetCursorPosition)
import System.IO
    ( hPutStr
    , stderr
    )

data MetricsEvent
    = BlockFetchMetrics EventQueueLength
    | UTxOChangesCount
    | CurrentBlockInfo Header

data Metrics = Metrics
    { averageQueueLength :: Double
    , maxQueueLength :: Int
    , utxoChanges :: Int
    , utxoSpeed :: Double
    , blockSpeed :: Double
    }

data MetricsState = MetricsState
    { msQueueLengthWindow :: [Int]
    , msLastUtxOChangesCount :: Int
    , msLastBlockInfo :: Maybe (Point, BlockNo)
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
updateMetrics _ ms metrics UTxOChangesCount =
    (metrics{utxoChanges = utxoChanges metrics + 1}, ms)
updateMetrics _ ms metrics (CurrentBlockInfo cbi) =
    ( metrics
    , ms{msLastBlockInfo = Just (blockPoint cbi, blockNo cbi)}
    )

metricsTracer :: Int -> IO (Tracer IO MetricsEvent)
metricsTracer l = do
    queue <- newTBQueueIO 100

    metricsState <-
        newTVarIO
            $ MetricsState
                { msQueueLengthWindow = []
                , msLastUtxOChangesCount = 0
                , msLastBlockInfo = Nothing
                }
    -- Initial metrics
    metricsVar <-
        newTVarIO
            $ Metrics
                { averageQueueLength = 0
                , maxQueueLength = 0
                , utxoChanges = 0
                , utxoSpeed = 0
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
        hClearScreen stderr
        hSetCursorPosition stderr 0 0
        hPutStr stderr
            $ "Average Queue Length: "
                ++ show (averageQueueLength metrics)
                ++ "\nMax Queue Length: "
                ++ show (maxQueueLength metrics)
                ++ "\nTotal utxo changes processed: "
                ++ show (utxoChanges metrics)
                ++ "\nUTXO Change Speed (utxo changes/sec): "
                ++ show @Double
                    ( let changes = utxoChanges metrics
                      in  fromIntegral
                            (changes - msLastUtxOChangesCount state)
                            / 10.0
                    )
                ++ case msLastBlockInfo state of
                    Nothing -> "\n"
                    Just point ->
                        "\nCurrent Block Point: " ++ renderBlockPoint point
                ++ "\nBlock Processing Speed (blocks/sec): "
                ++ show @Double
                    ( let mbLast = msLastBlockInfo state
                      in  case mbLast of
                            Nothing -> 0
                            Just (_, BlockNo lastH) ->
                                let currentH = case msLastBlockInfo state of
                                        Nothing -> lastH
                                        Just (_, BlockNo h) -> h
                                in  fromIntegral
                                        (currentH - lastH)
                                        / 10.0
                    )
        atomically $ do
            modifyTVar metricsState $ \s ->
                s{msLastUtxOChangesCount = utxoChanges metrics}
        reportLoop
    pure tracer

renderBlockPoint :: (Point, BlockNo) -> String
renderBlockPoint (Network.Point Origin, _) = "Origin\n"
renderBlockPoint (Network.Point (At block), BlockNo h) =
    show (blockPointHash block)
        ++ "@"
        ++ show (unSlotNo $ blockPointSlot block)
        ++ "\n"
        ++ "Block No: "
        ++ show h
