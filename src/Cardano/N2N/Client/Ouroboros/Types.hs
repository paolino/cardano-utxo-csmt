module Cardano.N2N.Client.Ouroboros.Types
    ( Block
    , Header
    , HeaderHash
    , Tip
    , Point
    , BlockFetchApplication
    , ChainSyncApplication
    , KeepAliveApplication
    , ChainSync
    , BlockFetch
    , Follower (..)
    , ProgressOrRewind (..)
    , Intersector (..)
    ) where

import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient)
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient)
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.KeepAlive.Client (KeepAliveClient)

-- | Real Cardano Block type
type Block = Consensus.CardanoBlock Consensus.StandardCrypto

-- | Real Cardano Header type
type Header = Consensus.CardanoHeader Consensus.StandardCrypto

-- | Real Cardano Tip type
type Tip = Network.Tip Block

-- | Real Cardano Point type
type Point = Network.Point Header

-- | The header hash type used in the chain sync connection
type HeaderHash = Network.HeaderHash Block

-- | Type alias for BlockFetch application
type BlockFetchApplication = BlockFetchClient Block Point IO ()

-- | Type alias for ChainSync application
type ChainSyncApplication = ChainSyncClient Header Point Tip IO ()

-- | Type alias for KeepAlive protocol
type KeepAliveApplication = KeepAliveClient IO ()

-- | Type alias for ChainSync protocol
type ChainSync = ChainSync.ChainSync Header Point Tip

-- | Type alias for BlockFetch protocol
type BlockFetch = BlockFetch.BlockFetch Block Point

data ProgressOrRewind h
    = Progress (Follower h)
    | Rewind [Point] (Intersector h)

-- | An event representing a roll forward or roll backward in the chain
data Follower h = Follower
    { rollForward :: h -> IO (Follower h)
    , rollBackward :: Point -> IO (ProgressOrRewind h)
    }

data Intersector h = Intersector
    { intersectFound :: Point -> IO (Follower h)
    , intersectNotFound :: IO (Intersector h, [Point])
    }
