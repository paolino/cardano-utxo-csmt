module Cardano.UTxOCSMT.Application.Run.Application
    ( application
    , ApplicationTrace (..)
    , renderApplicationTrace
    )
where

import CSMT ()
import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength
    , Fetched (..)
    , mkBlockFetchApplication
    )
import Cardano.UTxOCSMT.Application.ChainSync
    ( mkChainSyncApplication
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , Update (..)
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( MetricsEvent (..)
    )
import Cardano.UTxOCSMT.Application.UTxOs (Change (..), uTxOs)
import Cardano.UTxOCSMT.Ouroboros.Connection (runNodeApplication)
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Follower (..)
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    )
import Control.Exception (throwIO)
import Control.Monad (replicateM_)
import Control.Tracer (Tracer)
import Data.ByteString.Lazy (ByteString)
import Data.Function (fix)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Data.Void (Void)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (PortNumber)
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- | Events emitted by the application
data ApplicationTrace
    = ApplicationIntersectionAt Point
    | ApplicationIntersectionFailed
    | ApplicationRollingBack Point
    | -- | Block processed at slot with UTxO change count
      ApplicationBlockProcessed SlotNo Int

-- | Render an 'ApplicationTrace'
renderApplicationTrace :: ApplicationTrace -> String
renderApplicationTrace (ApplicationIntersectionAt point) =
    "Intersected at point: " ++ show point
renderApplicationTrace ApplicationIntersectionFailed =
    "Intersection failed, resetting to origin"
renderApplicationTrace (ApplicationRollingBack point) =
    "Rolling back to point: " ++ show point
renderApplicationTrace (ApplicationBlockProcessed slot utxoCount) =
    "Block processed: slot "
        ++ show (unSlotNo slot)
        ++ ", "
        ++ show utxoCount
        ++ " UTxO changes"

origin :: Network.Point block
origin = Network.Point{getPoint = Origin}

type DBState = State IO Point ByteString ByteString

intersector
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> IO (Maybe Point)
    -> DBState
    -> Intersector Fetched
intersector TraceWith{trace, tracer} trUTxO mFinality updater =
    Intersector
        { intersectFound = \point -> do
            trace $ ApplicationIntersectionAt point
            pure $ follower tracer trUTxO mFinality updater
        , intersectNotFound = do
            trace ApplicationIntersectionFailed
            pure
                ( intersector tracer trUTxO mFinality updater
                , [origin]
                )
        }

changeToOperation :: Change -> Operation ByteString ByteString
changeToOperation (Spend k) = Delete k
changeToOperation (Create k v) = Insert k v

follower
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> IO (Maybe Point)
    -> DBState
    -> Follower Fetched
follower
    TraceWith{trace, tracer}
    trUTxO
    newFinalityTarget
    db = ($ db) $ fix $ \go currentDB ->
        Follower
            { rollForward = \Fetched{fetchedPoint, fetchedBlock} -> do
                let ops = changeToOperation <$> uTxOs fetchedBlock
                    opsCount = length ops
                replicateM_ opsCount trUTxO
                -- Log progress every 1000 slots
                case Network.pointSlot fetchedPoint of
                    At slot@(SlotNo s)
                        | s `mod` 1000 == 0 ->
                            trace
                                $ ApplicationBlockProcessed slot opsCount
                    _ -> pure ()
                newDB <- case currentDB of
                    Syncing update -> do
                        newUpdate <- forwardTipApply update fetchedPoint ops
                        finality <- newFinalityTarget
                        Syncing <$> case finality of
                            Nothing -> pure newUpdate
                            Just slot -> forwardFinalityApply newUpdate slot
                    _ -> error "follower: cannot roll forward while intersecting"
                pure $ go newDB
            , rollBackward = \point ->
                rollingBack
                    tracer
                    trUTxO
                    newFinalityTarget
                    point
                    go
                    currentDB
            }

rollingBack
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> IO (Maybe Point)
    -> Point
    -> (DBState -> Follower Fetched)
    -> DBState
    -> IO (ProgressOrRewind Fetched)
rollingBack TraceWith{trace, tracer} trUTxO newFinalityTarget point follower' state = do
    trace $ ApplicationRollingBack point
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
                        $ intersector tracer trUTxO newFinalityTarget
                        $ Syncing newUpdate
                Intersecting ps newUpdate ->
                    Rewind ps
                        $ intersector tracer trUTxO newFinalityTarget
                        $ Syncing newUpdate
        _ -> error "rollingBack: cannot roll back while intersecting"

application
    :: NetworkMagic
    -- ^ Network magic
    -> String
    -- ^ Node name
    -> PortNumber
    -- ^ Port number
    -> Point
    -- ^ Starting point
    -> EventQueueLength
    -- ^ Headers queue size
    -> (Point -> IO ())
    -- ^ Action to set the base checkpoint
    -> Maybe SlotNo
    -- ^ Optional skip until slot for Mithril bootstrap
    -> Tracer IO MetricsEvent
    -- ^ Tracer for metrics events
    -> Tracer IO ApplicationTrace
    -- ^ Tracer for application events
    -> Update IO Point ByteString ByteString
    -- ^ Initial database FSM update
    -> [Point]
    -- ^ Available points to sync from
    -> IO (Maybe Point)
    -- ^ Finality target. TODO redesign the Update object to avoid this
    -> IO Void
application
    networkMagic
    nodeName
    portNumber
    startingPoint
    headersQueueSize
    setCheckpoint
    mSkipTargetSlot
    TraceWith{trace = metricTrace, contra = metricContra}
    TraceWith{tracer}
    initialDBUpdate
    availablePoints
    mFinality =
        do
            hSetBuffering stdout NoBuffering

            let counting = metricTrace UTxOChangeEvent

            (blockFetchApplication, headerIntersector) <-
                mkBlockFetchApplication
                    headersQueueSize
                    (metricContra BlockFetchEvent)
                    setCheckpoint
                    mSkipTargetSlot
                    $ intersector tracer counting mFinality
                    $ Syncing initialDBUpdate
            let chainFollowingApplication =
                    mkChainSyncApplication
                        (metricContra BlockInfoEvent)
                        (metricContra ChainTipEvent)
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
                Right (Left ()) ->
                    error "application: chain following application exited unexpectedly"
                Right _ -> error "application: impossible branch reached"
