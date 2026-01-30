{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.UTxOCSMT.Application.Options
    ( Options (..)
    , CardanoNetwork (..)
    , optionsParser
    , optionsParserCore

      -- * Derived option accessors
    , networkMagic

      -- * Re-exports for Mithril
    , MithrilOptions (..)
    , MithrilNetwork (..)
    )
where

import Autodocodec
    ( HasCodec (..)
    , dimapCodec
    , shownBoundedEnumCodec
    )

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
import Data.Word (Word16, Word64)
import Network.Socket (PortNumber)
import OptEnvConf
    ( Parser
    , auto
    , conf
    , filePathSetting
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
    , subConfig
    , switch
    , value
    , withYamlConfig
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
import Path (Abs, File, Path)
import Text.Read (readMaybe)

-- | Cardano network selection
data CardanoNetwork
    = Mainnet
    | Preprod
    | Preview
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

instance HasCodec CardanoNetwork where
    codec = shownBoundedEnumCodec

-- | Orphan instance for PortNumber (Word16 underneath)
instance HasCodec PortNumber where
    codec =
        dimapCodec fromIntegral (fromIntegral :: PortNumber -> Word16) codec

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

data Options = Options
    { network :: CardanoNetwork
    , nodeName :: String
    , portNumber :: PortNumber
    , startingPoint :: Point
    , headersQueueSize :: EventQueueLength
    , dbPath :: FilePath
    , logPath :: Maybe FilePath
    , apiPort :: Maybe PortNumber
    , apiDocsPort :: Maybe PortNumber
    , metricsOn :: Bool
    , mithrilOptions :: MithrilOptions
    , syncThreshold :: Word64
    -- ^ Number of slots behind chain tip to consider synced (default: 100)
    }

-- | Get effective network magic from options
networkMagic :: Options -> NetworkMagic
networkMagic = networkMagicFor . network

-- | Option to specify a YAML configuration file
configFileOption :: Parser (Maybe (Path Abs File))
configFileOption =
    optional
        $ filePathSetting
            [ long "config-file"
            , short 'c'
            , help "Path to YAML configuration file"
            , metavar "FILE"
            , option
            ]

dbPathOption :: Parser FilePath
dbPathOption =
    setting
        [ long "db-path"
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
        , conf "network"
        , help
            "Cardano network (mainnet, preprod, preview). \
            \Sets network magic, default peer node, and Mithril network."
        , metavar "NETWORK"
        , reader $ maybeReader readCardanoNetwork
        , value Mainnet
        , option
        ]

nodeNameOption :: Parser String
nodeNameOption =
    setting
        [ long "node-name"
        , short 's'
        , conf "node-name"
        , help "Peer node hostname"
        , metavar "HOSTNAME"
        , reader str
        , option
        ]

portNumberOption :: Parser PortNumber
portNumberOption =
    setting
        [ long "node-port"
        , short 'p'
        , conf "node-port"
        , help "Peer node port"
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

syncThresholdOption :: Parser Word64
syncThresholdOption =
    setting
        [ long "sync-threshold"
        , help
            "Number of slots behind chain tip to consider synced \
            \(default: 100, ~33 minutes)"
        , metavar "SLOTS"
        , value 100
        , reader auto
        , option
        ]

-- | Main options parser with YAML config file support
optionsParser :: Parser Options
optionsParser = withYamlConfig configFileOption optionsParserCore

-- | Core options parser (used by withYamlConfig)
optionsParserCore :: Parser Options
optionsParserCore =
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
        <*> subConfig "mithril" mithrilOptionsParser'
        <*> syncThresholdOption
  where
    mkOptions
        net
        node
        port
        start
        queue
        db
        logP
        api
        apiDocs
        metrics
        mithril
        threshold =
            Options
                { network = net
                , nodeName = node
                , portNumber = port
                , startingPoint = start
                , headersQueueSize = queue
                , dbPath = db
                , logPath = logP
                , apiPort = api
                , apiDocsPort = apiDocs
                , metricsOn = metrics
                , mithrilOptions = mithril{mithrilNetwork = mithrilNetworkFor net}
                , syncThreshold = threshold
                }
