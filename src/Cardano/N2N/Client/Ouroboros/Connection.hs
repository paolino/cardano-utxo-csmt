module Cardano.N2N.Client.Ouroboros.Connection
    ( runNodeApplication
    , ChainSyncApplication
    , BlockFetchApplication
    )
where

import Cardano.N2N.Client.Application.BlockFetch
    ( BlockFetchApplication
    )
import Cardano.N2N.Client.Application.KeepAlive (keepAliveApplication)
import Cardano.N2N.Client.Ouroboros.Application
    ( mkOuroborosApplication
    )
import Cardano.N2N.Client.Ouroboros.Types (ChainSyncApplication)
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
