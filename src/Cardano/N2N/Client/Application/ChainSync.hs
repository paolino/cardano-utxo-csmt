module Cardano.N2N.Client.Application.ChainSync
    ( mkChainSyncApplication
    , ChainSyncApplication
    , Event (..)
    )
where

import Cardano.N2N.Client.Ouroboros.Types
    ( ChainSyncApplication
    , Header
    , Point
    , Tip
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , StrictTBQueue
    , StrictTVar
    , readTVar
    , writeTBQueue
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

-- | An event representing a roll forward or roll backward in the chain
data Event = RollForward Header Tip | RollBackward Point Tip

-- The idle state of the chain sync client
type ChainSyncIdle = ClientStIdle Header Point Tip IO ()

-- | boots the protocol and step into initialise
mkChainSyncApplication
    :: StrictTBQueue IO Event
    -- ^ queue to write roll events to
    -> StrictTVar IO Bool
    -- ^ variable indicating whether we're done
    -> Point
    -- ^ starting point
    -> ChainSyncApplication
    -- ^ the chain sync client application
mkChainSyncApplication events doneVar startingPoint =
    ChainSyncClient . pure
        $ SendMsgFindIntersect [startingPoint, Network.Point Origin]
        $ ClientStIntersect
            { recvMsgIntersectFound = \_point _tip ->
                ChainSyncClient . pure $ requestNext events doneVar
            , recvMsgIntersectNotFound = \_tip ->
                ChainSyncClient . pure $ SendMsgDone ()
            }

requestNext
    :: StrictTBQueue IO Event
    -> StrictTVar IO Bool
    -> ChainSyncIdle
requestNext events doneVar = fix $ \go ->
    let sendEvent e = ChainSyncClient . atomically $ do
            writeTBQueue events e
            done <- readTVar doneVar
            pure
                $ if done
                    then SendMsgDone ()
                    else go
    in  SendMsgRequestNext
            (pure ()) -- spare time for other work
            ClientStNext
                { recvMsgRollForward = \header -> sendEvent . RollForward header
                , recvMsgRollBackward = \point -> sendEvent . RollBackward point
                }
