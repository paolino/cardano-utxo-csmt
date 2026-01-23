module Cardano.UTxOCSMT.Application.Options
    ( Options (..)
    , Limit (..)
    , networkMagicOption
    , nodeNameOption
    , portNumberOption
    , optionsParser
    )
where

import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength (..)
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
    , strOption
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

data Options = Options
    { networkMagic :: NetworkMagic
    , nodeName :: String
    , portNumber :: PortNumber
    , startingPoint :: Point
    , headersQueueSize :: EventQueueLength
    , dbPath :: FilePath
    , logPath :: Maybe FilePath
    , apiPort :: Maybe PortNumber
    , apiDocsPort :: Maybe PortNumber
    , metricsOn :: Bool
    }

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
            , help "Path to the log file"
            , metavar "FILE"
            , value "cardano-utxo-csmt.log"
            , reader str
            , option
            ]

networkMagicOption :: Parser NetworkMagic
networkMagicOption =
    NetworkMagic
        <$> setting
            [ long "network-magic"
            , short 'm'
            , help "Network magic number"
            , metavar "INT"
            , value 764824073
            , reader auto
            , option
            ]

nodeNameOption :: Parser String
nodeNameOption =
    strOption
        [ long "node-name"
        , short 's'
        , help "Peer node hostname"
        , metavar "HOSTNAME"
        , value "backbone.cardano.iog.io"
        , reader str
        , option
        ]

portNumberOption :: Parser PortNumber
portNumberOption =
    setting
        [ long "port"
        , short 'p'
        , help "Peer node port number"
        , metavar "INT"
        , value 3001
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
    Options
        <$> networkMagicOption
        <*> nodeNameOption
        <*> portNumberOption
        <*> startingPointOption
        <*> eventQueueSizeOption
        <*> dbPathOption
        <*> logPathOption
        <*> apiPortOption
        <*> apiDocsPortOption
        <*> metricsSwitch
