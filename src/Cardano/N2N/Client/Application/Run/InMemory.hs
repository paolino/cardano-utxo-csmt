{-# LANGUAGE NumericUnderscores #-}

module Cardano.N2N.Client.Application.Run.InMemory
    ( InMemoryUpdater
    , intersector
    , getRollbackPoints
    , main
    )
where

import CSMT ()
import Cardano.N2N.Client.Application.BlockFetch
    ( mkBlockFetchApplication
    )
import Cardano.N2N.Client.Application.ChainSync
    ( mkChainSyncApplication
    )
import Cardano.N2N.Client.Application.Database.InMemory
    ( InMemory (mInverseOps)
    , InMemoryState
    , emptyInMemory
    , mkInMemoryDatabaseSimple
    , updateInMemory
    )
import Cardano.N2N.Client.Application.Database.Interface
    ( Operation (..)
    , Truncated (..)
    , Update (..)
    )
import Cardano.N2N.Client.Application.Metrics
    ( Metrics (..)
    , MetricsEvent (..)
    , MetricsParams (..)
    , metricsTracer
    )
import Cardano.N2N.Client.Application.Options
    ( Options (..)
    , optionsParser
    )
import Cardano.N2N.Client.Application.SampleList (sampleList)
import Cardano.N2N.Client.Application.UTxOs (Change (..), uTxOs)
import Cardano.N2N.Client.Ouroboros.Connection (runNodeApplication)
import Cardano.N2N.Client.Ouroboros.Types
    ( Block
    , Follower (..)
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    )
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (..))
import Control.Concurrent.Class.MonadSTM.Strict.TVar
import Control.Exception (throwIO)
import Control.Monad.State.Strict
    ( StateT (..)
    )
import Control.Tracer
import Data.ByteString.Lazy (ByteString)
import Data.Function (fix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import OptEnvConf (runParser)
import Ouroboros.Network.Block (SlotNo (..), blockNo, blockPoint)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point
    ( WithOrigin (..)
    , blockPointHash
    , blockPointSlot
    )
import Paths_cardano_utxo_csmt (version)
import System.Console.ANSI
import System.IO (BufferMode (..), hSetBuffering, stdout)

type DB = StrictTVar IO (InMemory Point ByteString ByteString)

type Updater = IORef InMemoryUpdater

type InMemoryChain = InMemoryState IO Point ByteString ByteString

type InMemoryUpdater =
    Update
        InMemoryChain
        Point
        ByteString
        ByteString

getRollbackPoints :: DB -> IO (NonEmpty Point)
getRollbackPoints dbVar = do
    db <- readTVarIO dbVar
    case sampleList $ Map.keys $ mInverseOps db of
        [] -> pure $ Network.Point Origin :| []
        (p : ps) -> pure (p :| ps)

intersector :: DB -> Updater -> Intersector (Point, Block)
intersector db updater =
    Intersector
        { intersectFound = \point -> do
            putStrLn $ "Intersected at point: " ++ show point
            r <- rollingBack db updater point (follower db updater)
            case r of
                Progress f -> pure f
                Rewind _ _ -> error "intersector: intersectFound produced Rewind"
        , intersectNotFound =
            pure
                ( intersector db updater
                , [Network.Point Origin]
                )
        }

changeToOperation :: Change -> Operation ByteString ByteString
changeToOperation (Spend k) = Delete k
changeToOperation (Create k v) = Insert k v

follower :: DB -> Updater -> Follower (Point, Block)
follower db updater = fix $ \go ->
    Follower
        { rollForward = \(point, block) -> do
            let ops = changeToOperation <$> uTxOs block
            () <- onDb db updater $ \update -> do
                x <- forwardTipApply update point ops
                pure ((), x)
            pure go
        , rollBackward = \point -> rollingBack db updater point go
        }

rollingBack
    :: DB
    -> Updater
    -> Point
    -> Follower (Point, Block)
    -> IO (ProgressOrRewind (Point, Block))
rollingBack db updater point follower' = do
    putStrLn $ "Rolling back to point: " ++ show point
    c <- onDb db updater $ \update -> do
        result <- rollbackTipApply update (At point)
        case result of
            NotTruncated update' -> pure (False, update')
            Truncated truncated -> do
                pure (True, truncated)
    if c
        then pure $ Rewind [point] (intersector db updater)
        else pure $ Progress follower'

onDb
    :: DB
    -> Updater
    -> (InMemoryUpdater -> InMemoryChain (a, InMemoryUpdater))
    -> IO a
onDb dbVar updaterRef f = do
    updater <- readIORef updaterRef
    db <- readTVarIO dbVar
    ((a, updater'), db') <- flip runStateT db $ f updater
    writeIORef updaterRef updater'
    atomically $ writeTVar dbVar db'
    pure a

renderMetrics :: Metrics -> IO ()
renderMetrics
    Metrics
        { averageQueueLength
        , maxQueueLength
        , utxoChangesCount
        , lastBlockPoint
        , utxoSpeed
        , blockSpeed
        , currentEra
        } = do
        hClearScreen stdout
        hSetCursorPosition stdout 0 0
        putStrLn
            $ "Average Queue Length: "
                ++ show averageQueueLength
                ++ "\nMax Queue Length: "
                ++ show maxQueueLength
                ++ "\nTotal utxo changes processed: "
                ++ show utxoChangesCount
                ++ "\nUTXO Change Speed (utxo changes/sec): "
                ++ show utxoSpeed
                ++ "\nBlock Processing Speed (blocks/sec): "
                ++ show blockSpeed
                ++ "\nLast Block Point: "
                ++ maybe "N/A" renderBlockPoint lastBlockPoint
                ++ "\nLast Block Number: "
                ++ maybe "N/A" (show . blockNo . snd) lastBlockPoint
                ++ "\nLast Received Block Time: "
                ++ maybe "N/A" (show . fst) lastBlockPoint
                ++ "\nCurrent Era: "
                ++ fromMaybe "N/A" currentEra
      where
        renderBlockPoint (_, header) = case blockPoint header of
            Network.Point Origin -> "Origin"
            Network.Point (At block) ->
                show (blockPointHash block)
                    ++ "@"
                    ++ show (unSlotNo $ blockPointSlot block)

main :: IO ()
main = do
    options <-
        runParser
            version
            "Tracking cardano UTxOs in a CSMT in memory"
            optionsParser
    application options

application
    :: Options
    -> IO ()
application
    Options
        { networkMagic
        , nodeName
        , portNumber
        , startingPoint
        , headersQueueSize
        } = do
        hSetBuffering stdout NoBuffering
        tracer <-
            metricsTracer
                $ MetricsParams
                    { qlWindow = 100
                    , utxoSpeedWindow = 1000
                    , blockSpeedWindow = 100
                    , metricsOutput = renderMetrics
                    , metricsFrequency = 1_000_000
                    }
        -- let counting = traceWith tracer UTxOChangeEvent

        db <- newTVarIO emptyInMemory
        updater <- newIORef $ updateInMemory mkInMemoryDatabaseSimple
        (blockFetchApplication, headerIntersector) <-
            mkBlockFetchApplication
                headersQueueSize
                (contramap BlockFetchEvent tracer)
                $ intersector db updater
        let chainFollowingApplication =
                mkChainSyncApplication
                    (contramap BlockInfoEvent tracer)
                    headerIntersector
                    [startingPoint]
        result <-
            runNodeApplication
                networkMagic
                nodeName
                portNumber
                chainFollowingApplication
                blockFetchApplication

        case result of
            Left err -> throwIO err
            Right _ -> pure ()
