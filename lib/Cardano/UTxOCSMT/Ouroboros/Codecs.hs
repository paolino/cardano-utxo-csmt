module Cardano.UTxOCSMT.Ouroboros.Codecs
    ( codecChainSync
    , codecBlockFetch
    , codecKeepAlive
    , codecChainSyncN2C
    ) where

import Cardano.Chain.Slotting (EpochSlots (EpochSlots))
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Block
    , BlockFetch
    , ChainSync
    , Header
    , N2CChainSync
    , Point
    )
import Codec.Serialise (DeserialiseFailure, Serialise (..))
import Codec.Serialise.Decoding (Decoder)
import Codec.Serialise.Encoding (Encoding)
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Proxy (Proxy))
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Block.Abstract
    ( decodeRawHash
    , encodeRawHash
    )
import Ouroboros.Consensus.Byron.Ledger (ByronBlock, CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block
    ( CodecConfig (CardanoCodecConfig)
    )
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Node
    ( pattern CardanoNodeToClientVersion16
    , pattern CardanoNodeToNodeVersion2
    )
import Ouroboros.Consensus.HardFork.Combinator.NetworkVersion
    ( HardForkNodeToClientVersion
    , HardForkNodeToNodeVersion
    )
import Ouroboros.Consensus.Node.Serialisation
    ( decodeNodeToClient
    , decodeNodeToNode
    , encodeNodeToClient
    , encodeNodeToNode
    )
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger
    ( CodecConfig (ShelleyCodecConfig)
    )
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
    ( decodeTip
    , encodeTip
    )
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Protocol.BlockFetch.Codec qualified as BlockFetch
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.KeepAlive.Codec qualified as KeepAlive
import Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)

-- The ChainSync codec for our Block, Point, and Tip types
codecChainSync
    :: Codec
        ChainSync
        DeserialiseFailure
        IO
        LBS.ByteString
codecChainSync =
    ChainSync.codecChainSync
        encHeader
        decHeader
        encPoint
        decPoint
        encTip
        decTip

----- Encoding and Decoding Headers -----
encHeader :: Header -> Encoding
encHeader = encodeNodeToNode @Block ccfg version

decHeader :: Decoder s Header
decHeader = decodeNodeToNode @Block ccfg version

version
    :: HardForkNodeToNodeVersion
        (ByronBlock : Consensus.CardanoShelleyEras c)
version = CardanoNodeToNodeVersion2

ccfg :: Consensus.CardanoCodecConfig c
ccfg =
    CardanoCodecConfig
        (ByronCodecConfig $ EpochSlots 42)
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig

--- Encoding and Decoding Points -----
encPoint :: Point -> Encoding
encPoint = encode
decPoint :: Decoder s Point
decPoint = decode

--- Encoding and Decoding Tips -----
encTip :: Network.Tip Block -> Encoding
encTip = encodeTip (encodeRawHash (Proxy @Block))
decTip :: Decoder s (Network.Tip Block)
decTip = decodeTip (decodeRawHash (Proxy @Block))

-- | Real Cardano BlockFetch codec
codecBlockFetch
    :: Codec BlockFetch DeserialiseFailure IO LBS.ByteString
codecBlockFetch =
    BlockFetch.codecBlockFetch
        encBlock
        decBlock
        encPoint
        decPoint

-- ----- Encoding and Decoding Blocks -----
encBlock :: Block -> Encoding
encBlock = encodeNodeToNode @Block ccfg version

decBlock :: Decoder s Block
decBlock = decodeNodeToNode @Block ccfg version

codecKeepAlive
    :: Codec
        KeepAlive
        DeserialiseFailure
        IO
        LBS.ByteString
codecKeepAlive = KeepAlive.codecKeepAlive_v2

----- N2C (node-to-client) codecs -----

n2cVersion
    :: HardForkNodeToClientVersion
        (ByronBlock : Consensus.CardanoShelleyEras c)
n2cVersion = CardanoNodeToClientVersion16

-- | N2C ChainSync codec — encodes/decodes full blocks (not headers)
codecChainSyncN2C
    :: Codec
        N2CChainSync
        DeserialiseFailure
        IO
        LBS.ByteString
codecChainSyncN2C =
    ChainSync.codecChainSync
        encBlockN2C
        decBlockN2C
        encPointBlock
        decPointBlock
        encTip
        decTip

encBlockN2C :: Block -> Encoding
encBlockN2C = encodeNodeToClient @Block ccfg n2cVersion

decBlockN2C :: Decoder s Block
decBlockN2C = decodeNodeToClient @Block ccfg n2cVersion

encPointBlock :: Network.Point Block -> Encoding
encPointBlock = encode

decPointBlock :: Decoder s (Network.Point Block)
decPointBlock = decode
