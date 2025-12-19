module Cardano.N2N.Client.Application.ChainSync
    ( mkChainSyncApplication
    , ChainSyncApplication
    , Follower (..)
    , ProgressOrRewind (..)
    )
where

import Cardano.N2N.Client.Ouroboros.Types
    ( ChainSyncApplication
    , Follower (..)
    , Header
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    , Tip
    )
import Data.Function (fix)
import Ouroboros.Consensus.Cardano.Node ()
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
    :: Intersector Header
    -- ^ queue to write roll events to
    -> [Point]
    -- ^ starting point
    -> ChainSyncApplication
    -- ^ the chain sync client application
mkChainSyncApplication intersector startingPoints =
    ChainSyncClient $ pure $ intersect startingPoints intersector

intersect
    :: [Point]
    -> Intersector Header
    -> ClientStIdle Header Point Tip IO ()
intersect points Intersector{intersectFound, intersectNotFound} =
    SendMsgFindIntersect points
        $ ClientStIntersect
            { recvMsgIntersectFound = \point _ ->
                ChainSyncClient $ do
                    nextFollower <- intersectFound point
                    pure $ next nextFollower
            , recvMsgIntersectNotFound = \_ ->
                ChainSyncClient $ do
                    (intersector', points') <- intersectNotFound
                    pure $ intersect points' intersector'
            }

next
    :: Follower Header
    -> ChainSyncIdle
next follower = ($ follower)
    $ fix
    $ \go (Follower{rollForward, rollBackward}) ->
        let
            checkResult :: IO (ProgressOrRewind Header) -> ChainSyncApplication
            checkResult getProgressOrRewind = ChainSyncClient $ do
                progressOrRewind <- getProgressOrRewind
                case progressOrRewind of
                    Progress follower' -> pure $ go follower'
                    Rewind points follower' ->
                        pure $ intersect points follower'
        in
            SendMsgRequestNext
                (pure ()) -- spare time for other work
                ClientStNext
                    { recvMsgRollForward = \header _ -> do
                        checkResult $ Progress <$> rollForward header
                    , recvMsgRollBackward = \point _ ->
                        checkResult $ rollBackward point
                    }
