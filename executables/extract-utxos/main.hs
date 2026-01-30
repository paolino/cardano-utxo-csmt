{- |
Utility to extract UTxOs from a Mithril snapshot to a golden file.

Usage:
  extract-utxos --network preview --output golden.cbor --count 1000
-}
module Main (main) where

import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilNetwork (..)
    , defaultMithrilConfig
    , downloadSnapshotHttp
    , fetchLatestSnapshot
    )
import Cardano.UTxOCSMT.Mithril.Extraction
    ( extractUTxOsFromSnapshot
    )
import Cardano.UTxOCSMT.Mithril.Streaming (convertToCBOR)
import Codec.Serialise (serialise)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
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
    , runParser
    , setting
    , str
    , value
    )
import Paths_cardano_utxo_csmt (version)
import Streaming.Prelude qualified as S
import System.IO.Temp (withSystemTempDirectory)

data Options = Options
    { optNetwork :: MithrilNetwork
    , optAggregatorUrl :: String
    , optOutput :: FilePath
    , optCount :: Word64
    }

readNetwork :: String -> Maybe MithrilNetwork
readNetwork "mainnet" = Just MithrilMainnet
readNetwork "preprod" = Just MithrilPreprod
readNetwork "preview" = Just MithrilPreview
readNetwork _ = Nothing

-- | Get default aggregator URL for a network
defaultAggregatorUrl :: MithrilNetwork -> String
defaultAggregatorUrl MithrilMainnet =
    "https://aggregator.release-mainnet.api.mithril.network/aggregator"
defaultAggregatorUrl MithrilPreprod =
    "https://aggregator.release-preprod.api.mithril.network/aggregator"
defaultAggregatorUrl MithrilPreview =
    "https://aggregator.pre-release-preview.api.mithril.network/aggregator"

optionsParser :: Parser Options
optionsParser =
    mkOptions
        <$> setting
            [ long "network"
            , help "Mithril network: mainnet, preprod, preview"
            , metavar "NETWORK"
            , reader $ maybeReader readNetwork
            , value MithrilPreview
            , option
            ]
        <*> optional
            ( setting
                [ long "aggregator-endpoint"
                , help "Mithril aggregator endpoint URL"
                , metavar "URL"
                , reader str
                , option
                ]
            )
        <*> setting
            [ long "output"
            , help "Output file path"
            , metavar "FILE"
            , reader str
            , value "golden-utxos.cbor"
            , option
            ]
        <*> setting
            [ long "count"
            , help "Number of UTxOs to extract"
            , metavar "INT"
            , reader auto
            , value 1000
            , option
            ]
  where
    mkOptions net maybeUrl output count =
        Options
            { optNetwork = net
            , optAggregatorUrl = fromMaybe (defaultAggregatorUrl net) maybeUrl
            , optOutput = output
            , optCount = count
            }

main :: IO ()
main = do
    Options{optNetwork, optAggregatorUrl, optOutput, optCount} <-
        runParser version "Extract UTxOs from Mithril snapshot" optionsParser

    putStrLn
        $ "Extracting " ++ show optCount ++ " UTxOs from " ++ show optNetwork
    putStrLn $ "Output: " ++ optOutput

    manager <- newManager tlsManagerSettings

    withSystemTempDirectory "mithril-extract" $ \tmpDir -> do
        let config =
                defaultMithrilConfig manager optNetwork optAggregatorUrl tmpDir

        -- Fetch snapshot metadata
        putStrLn "Fetching snapshot metadata..."
        fetchResult <- fetchLatestSnapshot config
        case fetchResult of
            Left err -> fail $ "Failed to fetch snapshot: " ++ show err
            Right snapshot -> do
                putStrLn "Found snapshot, downloading..."

                -- Download snapshot
                downloadResult <- downloadSnapshotHttp nullTracer config snapshot
                case downloadResult of
                    Left err -> fail $ "Failed to download: " ++ show err
                    Right dbPath -> do
                        putStrLn $ "Extracting UTxOs from: " ++ dbPath

                        -- Extract UTxOs
                        extractResult <-
                            extractUTxOsFromSnapshot
                                nullTracer
                                dbPath
                                (collectUtxos optCount)

                        case extractResult of
                            Left err ->
                                fail $ "Extraction failed: " ++ show err
                            Right utxos -> do
                                putStrLn
                                    $ "Collected "
                                        ++ show (length utxos)
                                        ++ " UTxOs"

                                -- Write to file as CBOR list
                                let encoded = serialise utxos
                                LBS.writeFile optOutput encoded
                                putStrLn $ "Written to: " ++ optOutput

-- | Collect first N UTxOs as CBOR-encoded pairs
collectUtxos
    :: Word64
    -> S.Stream (S.Of (LBS.ByteString, LBS.ByteString)) IO ()
    -> IO [(LBS.ByteString, LBS.ByteString)]
collectUtxos maxCount = go 0 []
  where
    go count acc stream
        | count >= maxCount = pure (reverse acc)
        | otherwise = do
            result <- S.next stream
            case result of
                Left () -> pure (reverse acc)
                Right ((keyBs, valBs), rest) ->
                    -- Convert MemPack to CBOR using library function
                    case convertToCBOR keyBs valBs of
                        Nothing -> go count acc rest
                        Just pair -> go (count + 1) (pair : acc) rest
