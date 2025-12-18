module Cardano.N2N.Client.Application.ChainSync
    ( mkChainSyncApplication
    , ChainSyncApplication
    , Follower (..)
    , FollowerResult (..)
    )
where

import Cardano.N2N.Client.Ouroboros.Types
    ( ChainSyncApplication
    , Follower (..)
    , FollowerResult (..)
    , Header
    , Point
    , Tip
    )
import Data.Function (fix)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))
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
    :: Follower Header
    -- ^ queue to write roll events to
    -> Point
    -- ^ starting point
    -> ChainSyncApplication
    -- ^ the chain sync client application
mkChainSyncApplication follower startingPoint =
    ChainSyncClient
        $ pure
        $ intersect
            [ startingPoint
            , Network.Point{getPoint = Origin}
            ]
            follower

intersect
    :: [Point]
    -> Follower Header
    -> ClientStIdle Header Point Tip IO ()
intersect points follower =
    SendMsgFindIntersect points
        $ ClientStIntersect
            { recvMsgIntersectFound = \_point _tip ->
                ChainSyncClient $ pure $ next follower
            , recvMsgIntersectNotFound = \_tip ->
                ChainSyncClient . pure $ SendMsgDone ()
            }

next
    :: Follower Header
    -> ChainSyncIdle
next follower = ($ follower)
    $ fix
    $ \go (Follower{rollForward, rollBackward}) ->
        let
            checkResult :: IO (FollowerResult Header) -> ChainSyncApplication
            checkResult getFollowerResult = ChainSyncClient $ do
                followerResult <- getFollowerResult
                case followerResult of
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
