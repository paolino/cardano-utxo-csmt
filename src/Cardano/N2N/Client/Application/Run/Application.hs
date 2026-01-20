{-# LANGUAGE NumericUnderscores #-}

module Cardano.N2N.Client.Application.Run.Application
    ( application
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
import Cardano.N2N.Client.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , Update (..)
    )
import Cardano.N2N.Client.Application.Metrics
    ( MetricsEvent (..)
    , MetricsParams (..)
    , metricsTracer
    )
import Cardano.N2N.Client.Application.Options
    ( Options (..)
    )
import Cardano.N2N.Client.Application.Run.RenderMetrics
    ( renderMetrics
    )
import Cardano.N2N.Client.Application.UTxOs (Change (..), uTxOs)
import Cardano.N2N.Client.Ouroboros.Connection (runNodeApplication)
import Cardano.N2N.Client.Ouroboros.Types
    ( Follower (..)
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    )
import Control.Exception (throwIO)
import Control.Monad (replicateM_)
import Control.Tracer (Contravariant (contramap), traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Function (fix)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )
import System.IO (BufferMode (..), hSetBuffering, stdout)

origin :: Network.Point block
origin = Network.Point{getPoint = Origin}

type DBState = State IO Point ByteString ByteString

intersector
    :: IO () -> IO (Maybe Point) -> DBState -> Intersector Fetched
intersector trUTxO mFinality updater =
    Intersector
        { intersectFound = \point -> do
            putStrLn $ "Intersected at point: " ++ show point
            pure $ follower trUTxO mFinality updater
        , intersectNotFound = do
            putStrLn "Intersect not found, resetting to origin"
            pure
                ( intersector trUTxO mFinality updater
                , [origin]
                )
        }

changeToOperation :: Change -> Operation ByteString ByteString
changeToOperation (Spend k) = Delete k
changeToOperation (Create k v) = Insert k v

follower
    :: IO () -> IO (Maybe Point) -> DBState -> Follower Fetched
follower trUTxO newFinalityTarget db = ($ db) $ fix $ \go currentDB ->
    Follower
        { rollForward = \Fetched{fetchedPoint, fetchedBlock} -> do
            let ops = changeToOperation <$> uTxOs fetchedBlock
            -- putStrLn
            --     $ "Rolling forward to point: "
            --         ++ show point
            --         ++ " with "
            --         ++ show (length ops)
            --         ++ " UTxO changes"
            replicateM_ (length ops) trUTxO
            newDB <- case currentDB of
                Syncing update -> do
                    newUpdate <- forwardTipApply update fetchedPoint ops
                    finality <- newFinalityTarget
                    Syncing <$> case finality of
                        Nothing -> pure newUpdate
                        Just slot -> forwardFinalityApply newUpdate slot
                _ -> error "follower: cannot roll forward while intersecting"
            pure $ go newDB
        , rollBackward = \point -> rollingBack trUTxO newFinalityTarget point go currentDB
        }

rollingBack
    :: IO ()
    -> IO (Maybe Point)
    -> Point
    -> (DBState -> Follower Fetched)
    -> DBState
    -> IO (ProgressOrRewind Fetched)
rollingBack trUTxO newFinalityTarget point follower' state = do
    putStrLn $ "Rolling back to point: " ++ show point
    case state of
        Syncing update -> do
            newState <- rollbackTipApply update (At point)
            pure $ case newState of
                Syncing newUpdate ->
                    Progress
                        $ follower'
                        $ Syncing newUpdate
                Truncating newUpdate ->
                    Reset
                        $ intersector trUTxO newFinalityTarget
                        $ Syncing newUpdate
                Intersecting ps newUpdate ->
                    Rewind ps
                        $ intersector trUTxO newFinalityTarget
                        $ Syncing newUpdate
        _ -> error "rollingBack: cannot roll back while intersecting"

application
    :: Options
    -> Update IO Point ByteString ByteString
    -> [Point]
    -> IO (Maybe Point)
    -> IO ()
application
    Options
        { networkMagic
        , nodeName
        , portNumber
        , startingPoint
        , headersQueueSize
        }
    initialDBUpdate
    availablePoints
    mFinality =
        do
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

            (blockFetchApplication, headerIntersector) <-
                mkBlockFetchApplication
                    headersQueueSize
                    (contramap BlockFetchEvent tracer)
                    $ intersector counting mFinality
                    $ Syncing initialDBUpdate
            let chainFollowingApplication =
                    mkChainSyncApplication
                        (contramap BlockInfoEvent tracer)
                        headerIntersector
                        $ if null availablePoints
                            then
                                [startingPoint]
                            else availablePoints
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
