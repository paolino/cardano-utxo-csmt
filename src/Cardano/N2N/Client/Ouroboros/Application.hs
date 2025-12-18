{- HLINT ignore "Use const" -}
module Cardano.N2N.Client.Ouroboros.Application
    ( mkOuroborosApplication
    )
where

import Cardano.N2N.Client.Application.BlockFetch
    ( BlockFetchApplication
    )
import Cardano.N2N.Client.Ouroboros.Codecs
    ( codecBlockFetch
    , codecChainSync
    , codecKeepAlive
    )
import Cardano.N2N.Client.Ouroboros.Types
    ( ChainSyncApplication
    , KeepAliveApplication
    )
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Void (Void)
import Network.Mux qualified as Mx
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , MiniProtocolNum (MiniProtocolNum)
    , OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (InitiatorProtocolOnly)
    , StartOnDemandOrEagerly (StartOnDemand)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BlockFetch
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.KeepAlive.Client
    ( keepAliveClientPeer
    )

-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits
        { maximumIngressQueue = maxBound
        }

mkOuroborosApplication
    :: ChainSyncApplication
    -- ^ chainSync
    -> BlockFetchApplication
    -- ^ blockFetch
    -> KeepAliveApplication
    -- ^ keepAlive
    -> OuroborosApplicationWithMinimalCtx
        Mx.InitiatorMode
        addr
        LazyByteString
        IO
        ()
        Void
mkOuroborosApplication chainSyncApp blockFetchApp keepAliveApp =
    OuroborosApplication
        { getOuroborosApplication =
            [ MiniProtocol
                { miniProtocolNum = MiniProtocolNum 2
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = maximumMiniProtocolLimits
                , miniProtocolRun = runChainSync
                }
            , MiniProtocol
                { miniProtocolNum = MiniProtocolNum 3
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = maximumMiniProtocolLimits
                , miniProtocolRun = runFetchBlocks
                }
            , MiniProtocol
                { miniProtocolNum = MiniProtocolNum 8
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = maximumMiniProtocolLimits
                , miniProtocolRun = runKeepAlive
                }
            ]
        }
  where
    runChainSync = InitiatorProtocolOnly
        $ mkMiniProtocolCbFromPeer
        $ \_ctx ->
            ( nullTracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer chainSyncApp
            )
    runFetchBlocks = InitiatorProtocolOnly
        $ mkMiniProtocolCbFromPeer
        $ \_ctx ->
            ( nullTracer
            , codecBlockFetch
            , BlockFetch.blockFetchClientPeer blockFetchApp
            )
    runKeepAlive = InitiatorProtocolOnly
        $ mkMiniProtocolCbFromPeer
        $ \_ctx ->
            ( nullTracer
            , codecKeepAlive
            , keepAliveClientPeer keepAliveApp
            )
