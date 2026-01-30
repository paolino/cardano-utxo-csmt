{-# LANGUAGE BangPatterns #-}

{- |
Memory test for streaming UTxO extraction.

Streams through all UTxOs from a Mithril snapshot without collecting them,
to verify memory usage stays constant.

Usage:
  memory-test --network preprod +RTS -s -RTS
-}
module Main (main) where

import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilNetwork (..)
    , MithrilTrace
    , defaultMithrilConfig
    , downloadSnapshotHttp
    , fetchLatestSnapshot
    , renderMithrilTrace
    )
import Cardano.UTxOCSMT.Mithril.Extraction
    ( ExtractionTrace (..)
    , extractUTxOsFromSnapshot
    , renderExtractionTrace
    )
import Control.Monad (when)
import Control.Tracer (Tracer, contramap, stdoutTracer)
import Data.Maybe (fromMaybe)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OptEnvConf
    ( Parser
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
import System.IO (hFlush, stdout)

data Options = Options
    { optNetwork :: MithrilNetwork
    , optAggregatorUrl :: String
    , optTmpDir :: FilePath
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
            , value MithrilPreprod
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
            [ long "tmp-dir"
            , help "Temporary directory for snapshot"
            , metavar "DIR"
            , reader str
            , value "/tmp/mithril-memory-test"
            , option
            ]
  where
    mkOptions net maybeUrl tmpDir =
        Options
            { optNetwork = net
            , optAggregatorUrl = fromMaybe (defaultAggregatorUrl net) maybeUrl
            , optTmpDir = tmpDir
            }

tracer :: Tracer IO ExtractionTrace
tracer = contramap renderExtractionTrace stdoutTracer

mithrilTracer :: Tracer IO MithrilTrace
mithrilTracer = contramap renderMithrilTrace stdoutTracer

main :: IO ()
main = do
    Options{optNetwork, optAggregatorUrl, optTmpDir} <-
        runParser version "Memory test for streaming extraction" optionsParser

    putStrLn $ "Network: " ++ show optNetwork
    putStrLn $ "Tmp dir: " ++ optTmpDir
    putStrLn ""

    manager <- newManager tlsManagerSettings
    let config =
            defaultMithrilConfig manager optNetwork optAggregatorUrl optTmpDir

    -- Fetch snapshot metadata
    putStrLn "Fetching snapshot metadata..."
    hFlush stdout
    fetchResult <- fetchLatestSnapshot config
    case fetchResult of
        Left err -> fail $ "Failed to fetch snapshot: " ++ show err
        Right snapshot -> do
            putStrLn "Found snapshot, downloading..."
            hFlush stdout

            -- Download snapshot
            downloadResult <- downloadSnapshotHttp mithrilTracer config snapshot
            case downloadResult of
                Left err -> fail $ "Failed to download: " ++ show err
                Right dbPath -> do
                    putStrLn $ "Snapshot at: " ++ dbPath
                    putStrLn ""
                    putStrLn "Streaming UTxOs (counting only, no collection)..."
                    hFlush stdout

                    startTime <- getCurrentTime

                    -- Extract and count UTxOs without collecting
                    extractResult <-
                        extractUTxOsFromSnapshot
                            tracer
                            dbPath
                            countUtxos

                    endTime <- getCurrentTime
                    let elapsed = diffUTCTime endTime startTime

                    case extractResult of
                        Left err ->
                            fail $ "Extraction failed: " ++ show err
                        Right (count, slot) -> do
                            putStrLn ""
                            putStrLn "=== Results ==="
                            putStrLn $ "Total UTxOs: " ++ show count
                            putStrLn $ "Slot: " ++ show slot
                            putStrLn
                                $ "Time: "
                                    ++ show elapsed
                            putStrLn
                                $ "Rate: "
                                    ++ show
                                        ( fromIntegral count
                                            / realToFrac elapsed
                                            :: Double
                                        )
                                    ++ " UTxOs/sec"
                            putStrLn ""
                            putStrLn "Check RTS stats above for memory usage."

-- | Count UTxOs without collecting them
countUtxos
    :: S.Stream (S.Of (a, b)) IO ()
    -> IO Word64
countUtxos = go 0
  where
    go !count stream = do
        result <- S.next stream
        case result of
            Left () -> pure count
            Right (_, rest) -> do
                -- Progress every 100k
                let newCount = count + 1
                when (newCount `mod` 100000 == 0) $ do
                    putStrLn $ "  " ++ show newCount ++ " UTxOs..."
                    hFlush stdout
                go newCount rest
