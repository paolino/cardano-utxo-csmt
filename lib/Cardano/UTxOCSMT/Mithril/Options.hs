{- |
Module      : Cardano.UTxOCSMT.Mithril.Options
Description : CLI options for Mithril bootstrap configuration

This module provides command-line option parsing for Mithril-based
bootstrapping of the UTxO CSMT database. Options include:

* Enable/disable Mithril bootstrapping
* Network selection (mainnet, preprod, preview)
* Custom aggregator URL override
* Path to mithril-client binary
-}
module Cardano.UTxOCSMT.Mithril.Options
    ( MithrilOptions (..)
    , mithrilOptionsParser
    , mithrilEnabledSwitch
    , mithrilNetworkOption
    )
where

import Cardano.UTxOCSMT.Mithril.Client (MithrilNetwork (..))
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
    , str
    , switch
    , value
    )

-- | Complete Mithril bootstrap options
data MithrilOptions = MithrilOptions
    { mithrilEnabled :: Bool
    -- ^ Whether to attempt Mithril bootstrap for empty databases
    , mithrilBootstrapOnly :: Bool
    -- ^ Exit after bootstrap, don't start chain sync
    , mithrilNetwork :: MithrilNetwork
    -- ^ Target Mithril network
    , mithrilAggregatorUrl :: Maybe String
    -- ^ Override aggregator URL (uses network default if Nothing)
    , mithrilClientPath :: FilePath
    -- ^ Path to mithril-client binary
    , mithrilDownloadDir :: Maybe FilePath
    -- ^ Directory for snapshot downloads (uses temp dir if Nothing)
    }
    deriving stock (Show, Eq)

-- | Parse Mithril network from string
readMithrilNetwork :: String -> Maybe MithrilNetwork
readMithrilNetwork "mainnet" = Just MithrilMainnet
readMithrilNetwork "preprod" = Just MithrilPreprod
readMithrilNetwork "preview" = Just MithrilPreview
readMithrilNetwork _ = Nothing

-- | Switch to enable Mithril bootstrapping
mithrilEnabledSwitch :: Parser Bool
mithrilEnabledSwitch =
    setting
        [ long "mithril-bootstrap"
        , help
            "Enable Mithril-based bootstrapping for empty databases. \
            \When enabled and the database is empty, attempts to download \
            \a certified Mithril snapshot instead of syncing from genesis."
        , reader auto
        , value False
        , switch True
        ]

-- | Switch to exit after bootstrap (don't start chain sync)
mithrilBootstrapOnlySwitch :: Parser Bool
mithrilBootstrapOnlySwitch =
    setting
        [ long "mithril-bootstrap-only"
        , help
            "Exit after Mithril bootstrap completes. \
            \Useful for testing or pre-populating databases without \
            \starting chain synchronization."
        , reader auto
        , value False
        , switch True
        ]

-- | Option to select Mithril network
mithrilNetworkOption :: Parser MithrilNetwork
mithrilNetworkOption =
    setting
        [ long "mithril-network"
        , help
            "Mithril network to use for bootstrapping. \
            \Options: mainnet, preprod, preview. \
            \Should match the Cardano network being synced."
        , metavar "NETWORK"
        , reader $ maybeReader readMithrilNetwork
        , value MithrilMainnet
        , option
        ]

-- | Option to override aggregator URL
mithrilAggregatorOption :: Parser (Maybe String)
mithrilAggregatorOption =
    optional
        $ setting
            [ long "mithril-aggregator"
            , help
                "Override Mithril aggregator URL. \
                \By default, uses the official aggregator for the selected network."
            , metavar "URL"
            , reader str
            , option
            ]

-- | Option to specify mithril-client binary path
mithrilClientPathOption :: Parser FilePath
mithrilClientPathOption =
    setting
        [ long "mithril-client-path"
        , help
            "Path to mithril-client binary. \
            \The mithril-client CLI handles snapshot download and verification."
        , metavar "PATH"
        , reader str
        , value "mithril-client"
        , option
        ]

-- | Option to specify download directory
mithrilDownloadDirOption :: Parser (Maybe FilePath)
mithrilDownloadDirOption =
    optional
        $ setting
            [ long "mithril-download-dir"
            , help
                "Directory for Mithril snapshot downloads. \
                \Uses a temporary directory if not specified."
            , metavar "DIR"
            , reader str
            , option
            ]

-- | Combined parser for all Mithril options
mithrilOptionsParser :: Parser MithrilOptions
mithrilOptionsParser =
    MithrilOptions
        <$> mithrilEnabledSwitch
        <*> mithrilBootstrapOnlySwitch
        <*> mithrilNetworkOption
        <*> mithrilAggregatorOption
        <*> mithrilClientPathOption
        <*> mithrilDownloadDirOption
