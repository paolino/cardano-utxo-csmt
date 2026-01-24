{- |
Module      : Cardano.UTxOCSMT.Application.ChainSync
Description : Chain synchronization client for Cardano node

This module implements the ChainSync mini-protocol client that connects
to a Cardano node and follows the chain. It handles:

* Finding intersection points with the local state
* Processing roll-forward events (new blocks)
* Processing roll-backward events (chain reorganizations)

The client uses a 'Follower' abstraction to decouple protocol handling
from actual database updates.
-}
module Cardano.UTxOCSMT.Application.ChainSync
    ( mkChainSyncApplication
    , ChainSyncApplication
    , Follower (..)
    , ProgressOrRewind (..)
    )
where

import Cardano.UTxOCSMT.Ouroboros.Types
    ( ChainSyncApplication
    , Follower (..)
    , Header
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    , Tip
    )
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (Origin))
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )

-- The idle state of the chain sync client
type ChainSyncIdle = ClientStIdle Header Point Tip IO ()

{- | Create a ChainSync client application.

The client will attempt to find an intersection with the node's chain
starting from the provided points, then follow the chain forward.
-}
mkChainSyncApplication
    :: Tracer IO Header
    -- ^ Tracer for logging received headers
    -> Intersector Header
    -- ^ Callback handlers for intersection results
    -> [Point]
    -- ^ Starting points to find intersection (usually tip + finality)
    -> ChainSyncApplication
mkChainSyncApplication tracer intersector startingPoints =
    ChainSyncClient $ pure $ intersect tracer startingPoints intersector

intersect
    :: Tracer IO Header
    -> [Point]
    -> Intersector Header
    -> ClientStIdle Header Point Tip IO ()
intersect tracer points Intersector{intersectFound, intersectNotFound} =
    SendMsgFindIntersect points
        $ ClientStIntersect
            { recvMsgIntersectFound = \point _ ->
                ChainSyncClient $ do
                    nextFollower <- intersectFound point
                    pure $ next tracer nextFollower
            , recvMsgIntersectNotFound = \_ ->
                ChainSyncClient $ do
                    (intersector', points') <- intersectNotFound
                    pure $ intersect tracer points' intersector'
            }

next
    :: Tracer IO Header
    -> Follower Header
    -> ChainSyncIdle
next tracer follower = ($ follower)
    $ fix
    $ \go (Follower{rollForward, rollBackward}) ->
        let
            checkResult :: IO (ProgressOrRewind Header) -> ChainSyncApplication
            checkResult getProgressOrRewind = ChainSyncClient $ do
                progressOrRewind <- getProgressOrRewind
                case progressOrRewind of
                    Progress follower' -> pure $ go follower'
                    Rewind points follower' ->
                        pure $ intersect tracer points follower'
                    Reset intersector' ->
                        pure $ intersect tracer [Network.Point Origin] intersector'
        in
            SendMsgRequestNext
                (pure ()) -- spare time for other work
                ClientStNext
                    { recvMsgRollForward = \header _ -> do
                        checkResult $ do
                            traceWith tracer header
                            Progress <$> rollForward header
                    , recvMsgRollBackward = \point _ ->
                        checkResult $ rollBackward point
                    }
