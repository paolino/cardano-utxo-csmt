{- |
Module      : Cardano.UTxOCSMT.Ouroboros.Connection
Description : Node-to-node connection establishment

This module handles connecting to a Cardano node using the node-to-node
protocol. It performs the version handshake and sets up the multiplexed
connection for running ChainSync, BlockFetch, and KeepAlive protocols.
-}
module Cardano.UTxOCSMT.Ouroboros.Connection
    ( runNodeApplication
    , ChainSyncApplication
    , BlockFetchApplication
    )
where

import Cardano.UTxOCSMT.Application.BlockFetch
    ( BlockFetchApplication
    )
import Cardano.UTxOCSMT.Application.KeepAlive (keepAliveApplication)
import Cardano.UTxOCSMT.Ouroboros.Application
    ( mkOuroborosApplication
    )
import Cardano.UTxOCSMT.Ouroboros.Types (ChainSyncApplication)
import Control.Exception (SomeException)
import Data.List.NonEmpty qualified as NE
import Data.Void (Void)
import Network.Socket
    ( AddrInfo (..)
    , AddrInfoFlag (AI_PASSIVE)
    , PortNumber
    , SocketType (Stream)
    , defaultHints
    , getAddrInfo
    )
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Diffusion.Configuration
    ( PeerSharing (PeerSharingDisabled)
    )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (InitiatorOnlyDiffusionMode)
    , NodeToNodeVersion (NodeToNodeV_14)
    , NodeToNodeVersionData (..)
    , nodeToNodeCodecCBORTerm
    )
import Ouroboros.Network.Protocol.Handshake.Codec
    ( cborTermVersionDataCodec
    , noTimeLimitsHandshake
    , nodeToNodeHandshakeCodec
    )
import Ouroboros.Network.Protocol.Handshake.Version
    ( Acceptable (acceptableVersion)
    , Queryable (queryVersion)
    , simpleSingletonVersions
    )
import Ouroboros.Network.Snocket
    ( makeSocketBearer
    , socketSnocket
    )
import Ouroboros.Network.Socket
    ( ConnectToArgs (..)
    , HandshakeCallbacks (..)
    , connectToNode
    , nullNetworkConnectTracers
    )

-- | Connect to a node-to-node chain sync server and run the given application
runNodeApplication
    :: NetworkMagic
    -- ^ The network magic
    -> String
    -- ^ host
    -> PortNumber
    -- ^ port
    -> ChainSyncApplication
    -- ^ application
    -> BlockFetchApplication
    -- ^ application
    -> IO (Either SomeException (Either () Void))
runNodeApplication
    magic
    peerName
    peerPort
    chainSyncApplication
    blockFetchApplication = withIOManager $ \iocp -> do
        AddrInfo{addrAddress} <- resolve peerName peerPort
        connectToNode -- withNode
            (socketSnocket iocp) -- TCP
            makeSocketBearer
            ConnectToArgs
                { ctaHandshakeCodec = nodeToNodeHandshakeCodec
                , ctaHandshakeTimeLimits = noTimeLimitsHandshake
                , ctaVersionDataCodec =
                    cborTermVersionDataCodec
                        nodeToNodeCodecCBORTerm
                , ctaConnectTracers = nullNetworkConnectTracers
                , ctaHandshakeCallbacks =
                    HandshakeCallbacks
                        { acceptCb = acceptableVersion
                        , queryCb = queryVersion
                        }
                }
            mempty -- socket options
            ( simpleSingletonVersions
                NodeToNodeV_14
                ( NodeToNodeVersionData
                    { networkMagic = magic
                    , diffusionMode = InitiatorOnlyDiffusionMode
                    , peerSharing = PeerSharingDisabled
                    , query = False
                    }
                )
                ( const
                    $ mkOuroborosApplication
                        chainSyncApplication
                        blockFetchApplication
                        keepAliveApplication
                )
            )
            Nothing
            addrAddress

resolve :: String -> PortNumber -> IO AddrInfo
resolve peerName peerPort = do
    let hints =
            defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
    NE.head
        <$> getAddrInfo (Just hints) (Just peerName) (Just $ show peerPort)
