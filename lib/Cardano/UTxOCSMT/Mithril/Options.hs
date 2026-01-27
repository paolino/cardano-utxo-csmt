{- |
Module      : Cardano.UTxOCSMT.Mithril.Options
Description : CLI options for Mithril bootstrap configuration

This module provides command-line option parsing for Mithril-based
bootstrapping of the UTxO CSMT database. Options include:

* Enable/disable Mithril bootstrapping
* Network selection (mainnet, preprod, preview)
* Custom aggregator URL override
* Path to mithril-client binary
* Ancillary verification key override
-}
module Cardano.UTxOCSMT.Mithril.Options
    ( MithrilOptions (..)
    , mithrilOptionsParser
    , mithrilOptionsParser'
    , mithrilEnabledSwitch
    )
where

import Cardano.UTxOCSMT.Mithril.Client (MithrilNetwork (..))
import Data.Text (Text)
import Data.Text qualified as T
import OptEnvConf
    ( Parser
    , auto
    , env
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
    -- ^ Aggregator URL (from env or CLI, falls back to network default)
    , mithrilGenesisVk :: Maybe Text
    -- ^ Genesis verification key for mithril-client CLI (from env or CLI)
    , mithrilClientPath :: FilePath
    -- ^ Path to mithril-client binary
    , mithrilDownloadDir :: Maybe FilePath
    -- ^ Directory for snapshot downloads (uses temp dir if Nothing)
    , mithrilAncillaryVk :: Maybe Text
    -- ^ Ancillary verification key (from env or CLI)
    , mithrilSkipAncillaryVerification :: Bool
    -- ^ Skip Ed25519 ancillary verification (not recommended)
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

-- | Option for aggregator URL (from env or CLI)
mithrilAggregatorOption :: Parser (Maybe String)
mithrilAggregatorOption =
    optional
        $ setting
            [ long "aggregator-endpoint"
            , env "AGGREGATOR_ENDPOINT"
            , help
                "Mithril aggregator endpoint URL. \
                \Required for Mithril bootstrap. Uses network default if not set. \
                \See: https://mithril.network/doc/manual/getting-started/network-configurations"
            , metavar "URL"
            , reader str
            , option
            ]

-- | Option for genesis verification key (from env or CLI)
mithrilGenesisVkOption :: Parser (Maybe Text)
mithrilGenesisVkOption =
    optional
        $ setting
            [ long "genesis-verification-key"
            , env "GENESIS_VERIFICATION_KEY"
            , help
                "Genesis verification key for mithril-client CLI (JSON-hex format). \
                \Required when using mithril-client binary for STM verification. \
                \Get official keys from mithril-infra/configuration/{network}/genesis.vkey"
            , metavar "KEY"
            , reader (T.pack <$> str)
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

-- | Option for ancillary verification key (from env or CLI)
mithrilAncillaryVkOption :: Parser (Maybe Text)
mithrilAncillaryVkOption =
    optional
        $ setting
            [ long "ancillary-verification-key"
            , env "ANCILLARY_VERIFICATION_KEY"
            , help
                "Ed25519 ancillary verification key (JSON-hex format). \
                \Required for Ed25519 verification of Mithril ancillary files. \
                \The key is used to verify the signature on ancillary_manifest.json. \
                \Get official keys from mithril-infra/configuration/{network}/ancillary.vkey"
            , metavar "KEY"
            , reader (T.pack <$> str)
            , option
            ]

-- | Switch to skip ancillary verification
mithrilSkipAncillaryVerificationSwitch :: Parser Bool
mithrilSkipAncillaryVerificationSwitch =
    setting
        [ long "mithril-skip-ancillary-verification"
        , help
            "Skip Ed25519 verification of ancillary manifest. \
            \NOT RECOMMENDED: disables cryptographic verification of \
            \downloaded ledger state files."
        , reader auto
        , value False
        , switch True
        ]

-- | Combined parser for all Mithril options
mithrilOptionsParser :: Parser MithrilOptions
mithrilOptionsParser =
    MithrilOptions
        <$> mithrilEnabledSwitch
        <*> mithrilBootstrapOnlySwitch
        <*> mithrilNetworkOption
        <*> mithrilAggregatorOption
        <*> mithrilGenesisVkOption
        <*> mithrilClientPathOption
        <*> mithrilDownloadDirOption
        <*> mithrilAncillaryVkOption
        <*> mithrilSkipAncillaryVerificationSwitch

{- | Parser for Mithril options without network selection.

The network is set from the main --network option and injected after parsing.
-}
mithrilOptionsParser' :: Parser MithrilOptions
mithrilOptionsParser' =
    MithrilOptions
        <$> mithrilEnabledSwitch
        <*> mithrilBootstrapOnlySwitch
        <*> pure MithrilMainnet -- Placeholder, will be overwritten
        <*> mithrilAggregatorOption
        <*> mithrilGenesisVkOption
        <*> mithrilClientPathOption
        <*> mithrilDownloadDirOption
        <*> mithrilAncillaryVkOption
        <*> mithrilSkipAncillaryVerificationSwitch
