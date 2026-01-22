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

-- | boots the protocol and step into initialise
mkChainSyncApplication
    :: Tracer IO Header
    -> Intersector Header
    -- ^ queue to write roll events to
    -> [Point]
    -- ^ starting point
    -> ChainSyncApplication
    -- ^ the chain sync client application
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
