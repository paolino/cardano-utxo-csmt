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
    ( Fetched (..)
    , mkBlockFetchApplication
    )
import Cardano.N2N.Client.Application.ChainSync
    ( mkChainSyncApplication
    )
import Cardano.N2N.Client.Application.Database.InMemory
    ( InMemory (mInverseOps)
    , InMemoryState
    , emptyInMemory
    , mkInMemoryDatabaseSimple
    , newFinality
    , updateInMemory
    )
import Cardano.N2N.Client.Application.Database.Interface
    ( Operation (..)
    , State (..)
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
    ( Follower (..)
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    )
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (..))
import Control.Concurrent.Class.MonadSTM.Strict.TVar
    ( StrictTVar
    , newTVarIO
    , readTVarIO
    , writeTVar
    )
import Control.Exception (throwIO)
import Control.Monad (replicateM_)
import Control.Monad.State.Strict
    ( StateT (..)
    )
import Control.Tracer (Contravariant (contramap), traceWith)
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
import System.Console.ANSI (hClearScreen, hSetCursorPosition)
import System.IO (BufferMode (..), hSetBuffering, stdout)

type DB = StrictTVar IO (InMemory Point ByteString ByteString)

type Updater = IORef InMemoryUpdater

type InMemoryChain = InMemoryState IO Point ByteString ByteString

origin :: Network.Point block
origin = Network.Point{getPoint = Origin}

type InMemoryUpdater =
    State
        InMemoryChain
        Point
        ByteString
        ByteString

getRollbackPoints :: DB -> IO (NonEmpty Point)
getRollbackPoints dbVar = do
    db <- readTVarIO dbVar
    case sampleList $ Map.keys $ mInverseOps db of
        [] -> pure $ origin :| []
        (p : ps) -> pure (p :| ps)

intersector :: IO () -> DB -> Updater -> Intersector Fetched
intersector trUTxO db updater =
    Intersector
        { intersectFound = \point -> do
            putStrLn $ "Intersected at point: " ++ show point
            pure $ follower trUTxO db updater
        , intersectNotFound = do
            putStrLn "Intersect not found, resetting to origin"
            pure
                ( intersector trUTxO db updater
                , [Network.Point Origin]
                )
        }

changeToOperation :: Change -> Operation ByteString ByteString
changeToOperation (Spend k) = Delete k
changeToOperation (Create k v) = Insert k v

follower :: IO () -> DB -> Updater -> Follower Fetched
follower trUTxO db updater = fix $ \go ->
    Follower
        { rollForward = \Fetched{fetchedPoint = point, fetchedBlock = block} -> do
            let ops = changeToOperation <$> uTxOs block
            -- putStrLn
            --     $ "Rolling forward to point: "
            --         ++ show point
            --         ++ " with "
            --         ++ show (length ops)
            --         ++ " UTxO changes"
            replicateM_ (length ops) trUTxO
            () <- onDb db updater $ \case
                Syncing update -> do
                    u1 <- forwardTipApply update point ops
                    mFinality <- newFinality 2160
                    u2 <- case mFinality of
                        Nothing -> pure u1
                        Just slot -> forwardFinalityApply u1 slot
                    pure ((), Syncing u2)
                _ -> error "follower: cannot roll forward while intersecting"
            pure go
        , rollBackward = \point -> rollingBack trUTxO db updater point go
        }

rollingBack
    :: IO ()
    -> DB
    -> Updater
    -> Point
    -> Follower Fetched
    -> IO (ProgressOrRewind Fetched)
rollingBack trUTxO db updater point follower' = do
    putStrLn $ "Rolling back to point: " ++ show point
    onDb db updater $ \case
        Syncing update -> do
            result <- rollbackTipApply update (At point)
            case result of
                Syncing{} -> pure (Progress follower', result)
                Truncating u ->
                    pure
                        (Reset (intersector trUTxO db updater), Syncing u)
                Intersecting ps u -> do
                    pure (Rewind ps (intersector trUTxO db updater), Syncing u)
        _ -> error "rollingBack: cannot roll back while intersecting"

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
        let counting = traceWith tracer UTxOChangeEvent

        db <- newTVarIO emptyInMemory
        updater <-
            newIORef $ Syncing $ updateInMemory mkInMemoryDatabaseSimple
        (blockFetchApplication, headerIntersector) <-
            mkBlockFetchApplication
                headersQueueSize
                (contramap BlockFetchEvent tracer)
                $ intersector counting db updater
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
