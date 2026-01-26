module Cardano.UTxOCSMT.Application.Options
    ( Options (..)
    , Limit (..)
    , CardanoNetwork (..)
    , optionsParser

      -- * Derived option accessors
    , networkMagic
    , nodeName
    , portNumber

      -- * Re-exports for Mithril
    , MithrilOptions (..)
    , MithrilNetwork (..)
    )
where

import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength (..)
    )
import Cardano.UTxOCSMT.Mithril.Client (MithrilNetwork (..))
import Cardano.UTxOCSMT.Mithril.Options
    ( MithrilOptions (..)
    , mithrilOptionsParser'
    )
import Cardano.UTxOCSMT.Ouroboros.Types (HeaderHash, Point)
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    )
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Short qualified as SBS
import Data.Word (Word32)
import Network.Socket (PortNumber)
import OptEnvConf
    ( Parser
    , auto
    , help
    , long
    , maybeReader
    , metavar
    , option
    , optional
    , reader
    , setting
    , short
    , str
    , switch
    , value
    )
import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import Ouroboros.Consensus.Cardano.Block
    ( CardanoShelleyEras
    , StandardCrypto
    )
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Point qualified as Network
import Ouroboros.Network.Point qualified as Network.Point
import Text.Read (readMaybe)

-- | A limit on the number of blocks to sync
newtype Limit = Limit {limit :: Word32}
    deriving newtype (Show, Read, Eq, Ord, Enum)

-- | Cardano network selection
data CardanoNetwork
    = Mainnet
    | Preprod
    | Preview
    deriving stock (Show, Eq, Ord)

-- | Get network magic for a Cardano network
networkMagicFor :: CardanoNetwork -> NetworkMagic
networkMagicFor Mainnet = NetworkMagic 764824073
networkMagicFor Preprod = NetworkMagic 1
networkMagicFor Preview = NetworkMagic 2

-- | Get Mithril network for a Cardano network
mithrilNetworkFor :: CardanoNetwork -> MithrilNetwork
mithrilNetworkFor Mainnet = MithrilMainnet
mithrilNetworkFor Preprod = MithrilPreprod
mithrilNetworkFor Preview = MithrilPreview

-- | Get default peer node for a Cardano network
defaultNodeFor :: CardanoNetwork -> String
defaultNodeFor Mainnet = "backbone.cardano.iog.io"
defaultNodeFor Preprod = "preprod-node.world.dev.cardano.org"
defaultNodeFor Preview = "preview-node.world.dev.cardano.org"

-- | Get default port for a Cardano network
defaultPortFor :: CardanoNetwork -> PortNumber
defaultPortFor Mainnet = 3001
defaultPortFor Preprod = 30000
defaultPortFor Preview = 30002

data Options = Options
    { network :: CardanoNetwork
    , nodeNameOverride :: Maybe String
    , portNumberOverride :: Maybe PortNumber
    , startingPoint :: Point
    , headersQueueSize :: EventQueueLength
    , dbPath :: FilePath
    , logPath :: Maybe FilePath
    , apiPort :: Maybe PortNumber
    , apiDocsPort :: Maybe PortNumber
    , metricsOn :: Bool
    , mithrilOptions :: MithrilOptions
    }

-- | Get effective network magic from options
networkMagic :: Options -> NetworkMagic
networkMagic = networkMagicFor . network

-- | Get effective node name from options
nodeName :: Options -> String
nodeName opts = case nodeNameOverride opts of
    Just name -> name
    Nothing -> defaultNodeFor (network opts)

-- | Get effective port number from options
portNumber :: Options -> PortNumber
portNumber opts = case portNumberOverride opts of
    Just port -> port
    Nothing -> defaultPortFor (network opts)

dbPathOption :: Parser FilePath
dbPathOption =
    setting
        [ long "csmt-db-path"
        , short 'd'
        , help "Path to the CSMT RocksDB database"
        , metavar "DIR"
        , reader str
        , option
        ]

logPathOption :: Parser (Maybe FilePath)
logPathOption =
    optional
        $ setting
            [ long "log-path"
            , short 'l'
            , help
                "Path to the log file (logs to stdout if not specified)"
            , metavar "FILE"
            , reader str
            , option
            ]

-- | Parse Cardano network from string
readCardanoNetwork :: String -> Maybe CardanoNetwork
readCardanoNetwork "mainnet" = Just Mainnet
readCardanoNetwork "preprod" = Just Preprod
readCardanoNetwork "preview" = Just Preview
readCardanoNetwork _ = Nothing

networkOption :: Parser CardanoNetwork
networkOption =
    setting
        [ long "network"
        , short 'n'
        , help
            "Cardano network (mainnet, preprod, preview). \
            \Sets network magic, default peer node, and Mithril network."
        , metavar "NETWORK"
        , reader $ maybeReader readCardanoNetwork
        , value Mainnet
        , option
        ]

nodeNameOption :: Parser (Maybe String)
nodeNameOption =
    optional
        $ setting
            [ long "node-name"
            , short 's'
            , help
                "Override peer node hostname \
                \(uses network default if not specified)"
            , metavar "HOSTNAME"
            , reader str
            , option
            ]

portNumberOption :: Parser (Maybe PortNumber)
portNumberOption =
    optional
        $ setting
            [ long "port"
            , short 'p'
            , help
                "Override peer node port \
                \(uses network default if not specified)"
            , metavar "INT"
            , reader auto
            , option
            ]

startingPointOption :: Parser Point
startingPointOption =
    Network.Point
        <$> setting
            [ long "starting-point"
            , short 'f'
            , help "Starting point to sync from (format: origin or slot number)"
            , metavar "POINT"
            , value Origin
            , reader $ maybeReader readChainPoint
            , option
            ]

metricsSwitch :: Parser Bool
metricsSwitch =
    setting
        [ long "enable-metrics-reporting"
        , help "Enable metrics reporting on stdout"
        , reader auto
        , value False
        , switch True
        ]

readChainPoint
    :: String
    -> Maybe
        ( WithOrigin
            ( Network.Point.Block
                SlotNo
                (OneEraHash (ByronBlock : CardanoShelleyEras StandardCrypto))
            )
        )
readChainPoint "origin" = Just Origin
readChainPoint string = case break (== '@') string of
    (blockHashStr, _ : slotNoStr) -> do
        (hash :: HeaderHash) <-
            either (const Nothing) (Just . OneEraHash . SBS.toShort)
                $ convertFromBase Base16
                $ B.pack blockHashStr
        slot <- SlotNo <$> readMaybe slotNoStr
        return $ At $ Network.Block slot hash
    _ -> Nothing

eventQueueSizeOption :: Parser EventQueueLength
eventQueueSizeOption =
    EventQueueLength
        <$> setting
            [ long "headers-queue-size"
            , short 'q'
            , help "Size of the headers queue"
            , metavar "INT"
            , value 10
            , reader auto
            , option
            ]

apiPortOption :: Parser (Maybe PortNumber)
apiPortOption =
    optional
        $ setting
            [ long "api-port"
            , help "Port number for the API server"
            , metavar "INT"
            , reader auto
            , option
            ]

apiDocsPortOption :: Parser (Maybe PortNumber)
apiDocsPortOption =
    optional
        $ setting
            [ long "api-docs-port"
            , help "Port number for the API documentation server"
            , metavar "INT"
            , reader auto
            , option
            ]

optionsParser :: Parser Options
optionsParser =
    mkOptions
        <$> networkOption
        <*> nodeNameOption
        <*> portNumberOption
        <*> startingPointOption
        <*> eventQueueSizeOption
        <*> dbPathOption
        <*> logPathOption
        <*> apiPortOption
        <*> apiDocsPortOption
        <*> metricsSwitch
        <*> mithrilOptionsParser'
  where
    mkOptions net nodeName' port' start queue db logP api apiDocs metrics mithril =
        Options
            { network = net
            , nodeNameOverride = nodeName'
            , portNumberOverride = port'
            , startingPoint = start
            , headersQueueSize = queue
            , dbPath = db
            , logPath = logP
            , apiPort = api
            , apiDocsPort = apiDocs
            , metricsOn = metrics
            , mithrilOptions = mithril{mithrilNetwork = mithrilNetworkFor net}
            }
