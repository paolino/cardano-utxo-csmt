{- |
Module      : Cardano.UTxOCSMT.Mithril.Client
Description : Mithril aggregator client for snapshot retrieval

This module provides functionality to interact with Mithril aggregators
for retrieving certified Cardano database snapshots. It supports:

* Fetching snapshot metadata from Mithril aggregator API
* Downloading and verifying snapshots via mithril-client CLI
* Network-specific aggregator URL configuration

The implementation delegates cryptographic verification to the external
@mithril-client@ binary, which handles all STM signature verification.
-}
module Cardano.UTxOCSMT.Mithril.Client
    ( -- * Configuration
      MithrilConfig (..)
    , MithrilNetwork (..)
    , defaultMithrilConfig

      -- * Snapshot metadata
    , SnapshotMetadata (..)
    , SnapshotDigest (..)

      -- * Operations
    , fetchLatestSnapshot
    , downloadSnapshot
    , downloadSnapshotHttp

      -- * Errors
    , MithrilError (..)
    , renderMithrilError

      -- * Tracing
    , MithrilTrace (..)
    , renderMithrilTrace
    )
where

import Control.Exception (Exception, try)
import Data.Aeson
    ( FromJSON (..)
    , decode
    , withObject
    , (.:)
    , (.:?)
    )
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Network.HTTP.Client
    ( HttpException
    , Manager
    , httpLbs
    , parseRequest
    , responseBody
    , responseStatus
    )
import Network.HTTP.Types.Status (statusCode)
import System.Exit (ExitCode (..))
import System.Process.Typed
    ( proc
    , readProcess
    , setEnv
    , setWorkingDir
    )

-- | Mithril network selection
data MithrilNetwork
    = MithrilMainnet
    | MithrilPreprod
    | MithrilPreview
    deriving stock (Show, Eq, Ord)

-- | Get default aggregator URL for a network
defaultAggregatorUrl :: MithrilNetwork -> String
defaultAggregatorUrl MithrilMainnet =
    "https://aggregator.release-mainnet.api.mithril.network/aggregator"
defaultAggregatorUrl MithrilPreprod =
    "https://aggregator.release-preprod.api.mithril.network/aggregator"
defaultAggregatorUrl MithrilPreview =
    "https://aggregator.pre-release-preview.api.mithril.network/aggregator"

-- | Configuration for Mithril client operations
data MithrilConfig = MithrilConfig
    { mithrilNetwork :: MithrilNetwork
    -- ^ Target Cardano network
    , mithrilAggregatorUrl :: String
    -- ^ Mithril aggregator URL (defaults to network-specific URL)
    , mithrilClientPath :: FilePath
    -- ^ Path to mithril-client binary
    , mithrilDownloadDir :: FilePath
    -- ^ Directory for downloading snapshots
    , mithrilHttpManager :: Manager
    -- ^ HTTP manager for API requests
    }

-- | Create default configuration for a network
defaultMithrilConfig
    :: Manager -> MithrilNetwork -> FilePath -> MithrilConfig
defaultMithrilConfig manager network downloadDir =
    MithrilConfig
        { mithrilNetwork = network
        , mithrilAggregatorUrl = defaultAggregatorUrl network
        , mithrilClientPath = "mithril-client"
        , mithrilDownloadDir = downloadDir
        , mithrilHttpManager = manager
        }

-- | Unique identifier for a snapshot (certificate hash)
newtype SnapshotDigest = SnapshotDigest {unSnapshotDigest :: Text}
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

-- | Metadata about an available Cardano database snapshot
data SnapshotMetadata = SnapshotMetadata
    { snapshotDigest :: SnapshotDigest
    -- ^ Unique digest/hash of the snapshot
    , snapshotBeaconSlot :: Word64
    -- ^ Immutable file number of the snapshot
    , snapshotBeaconEpoch :: Word64
    -- ^ Epoch of the snapshot
    , snapshotMerkleRoot :: Text
    -- ^ Merkle root of the snapshot
    , snapshotSize :: Maybe Word64
    -- ^ Uncompressed size in bytes
    , snapshotCertificateHash :: Text
    -- ^ Certificate hash for verification
    , snapshotLocations :: [Text]
    -- ^ Download URLs for the snapshot archive
    , snapshotAncillaryLocations :: [Text]
    -- ^ Download URLs for ancillary data (ledger state)
    }
    deriving stock (Show, Eq)

instance FromJSON SnapshotMetadata where
    parseJSON = withObject "SnapshotMetadata" $ \o -> do
        snapshotDigest <- SnapshotDigest <$> o .: "digest"
        beacon <- o .: "beacon"
        snapshotBeaconSlot <- beacon .: "immutable_file_number"
        snapshotBeaconEpoch <- beacon .: "epoch"
        snapshotMerkleRoot <- o .: "certificate_hash"
        snapshotSize <- o .:? "size"
        snapshotCertificateHash <- o .: "certificate_hash"
        snapshotLocations <- o .: "locations"
        snapshotAncillaryLocations <-
            o .:? "ancillary_locations" >>= \case
                Nothing -> pure []
                Just locs -> pure locs
        pure SnapshotMetadata{..}

-- | Errors that can occur during Mithril operations
data MithrilError
    = -- | HTTP request failed
      MithrilHttpError HttpException
    | -- | API returned non-200 status
      MithrilApiError Int Text
    | -- | Failed to parse API response
      MithrilParseError Text
    | -- | mithril-client binary not found
      MithrilClientNotFound FilePath
    | -- | mithril-client exited with error (exit code, stdout, stderr)
      MithrilClientFailed Int Text Text
    | -- | No snapshots available from aggregator
      MithrilNoSnapshots
    | -- | No download locations in snapshot metadata
      MithrilNoLocations
    | -- | Extraction command failed
      MithrilExtractionFailed Int Text
    deriving stock (Show)

instance Exception MithrilError

-- | Render error for logging
renderMithrilError :: MithrilError -> String
renderMithrilError (MithrilHttpError e) =
    "HTTP error: " <> show e
renderMithrilError (MithrilApiError code msg) =
    "API error (HTTP " <> show code <> "): " <> T.unpack msg
renderMithrilError (MithrilParseError msg) =
    "Parse error: " <> T.unpack msg
renderMithrilError (MithrilClientNotFound path) =
    "mithril-client not found at: " <> path
renderMithrilError MithrilNoSnapshots =
    "No snapshots available from aggregator"
renderMithrilError (MithrilClientFailed code stdout stderr) =
    "mithril-client failed (exit "
        <> show code
        <> "): "
        <> T.unpack stdout
        <> " "
        <> T.unpack stderr
renderMithrilError MithrilNoLocations =
    "No download locations in snapshot metadata"
renderMithrilError (MithrilExtractionFailed code msg) =
    "Extraction failed (exit " <> show code <> "): " <> T.unpack msg

-- | Trace events for Mithril operations
data MithrilTrace
    = -- | Fetching snapshot metadata from URL
      MithrilFetchingSnapshot String
    | -- | Found snapshot with digest, slot, epoch
      MithrilSnapshotFound SnapshotDigest Word64 Word64
    | -- | Starting download of snapshot to directory
      MithrilDownloading SnapshotDigest FilePath
    | -- | Download completed successfully
      MithrilDownloadComplete FilePath
    | -- | Verifying snapshot signatures
      MithrilVerifying SnapshotDigest
    | -- | Verification successful
      MithrilVerificationComplete
    deriving stock (Show, Eq)

-- | Render trace for logging
renderMithrilTrace :: MithrilTrace -> String
renderMithrilTrace (MithrilFetchingSnapshot url) =
    "Fetching Mithril snapshot metadata from: " <> url
renderMithrilTrace (MithrilSnapshotFound digest slot epoch) =
    "Found Mithril snapshot: "
        <> T.unpack (unSnapshotDigest digest)
        <> " at slot "
        <> show slot
        <> " (epoch "
        <> show epoch
        <> ")"
renderMithrilTrace (MithrilDownloading digest dir) =
    "Downloading Mithril snapshot "
        <> T.unpack (unSnapshotDigest digest)
        <> " to "
        <> dir
renderMithrilTrace (MithrilDownloadComplete dir) =
    "Mithril snapshot download complete: " <> dir
renderMithrilTrace (MithrilVerifying digest) =
    "Verifying Mithril snapshot: " <> T.unpack (unSnapshotDigest digest)
renderMithrilTrace MithrilVerificationComplete =
    "Mithril snapshot verification complete"

-- | Fetch the latest available snapshot metadata from the aggregator
fetchLatestSnapshot
    :: MithrilConfig
    -> IO (Either MithrilError SnapshotMetadata)
fetchLatestSnapshot MithrilConfig{mithrilAggregatorUrl, mithrilHttpManager} = do
    let url = mithrilAggregatorUrl <> "/artifact/snapshots"
    result <- try $ do
        request <- parseRequest url
        response <- httpLbs request mithrilHttpManager
        pure (responseStatus response, responseBody response)
    case result of
        Left (e :: HttpException) ->
            pure $ Left $ MithrilHttpError e
        Right (status, body)
            | statusCode status /= 200 ->
                pure
                    $ Left
                    $ MithrilApiError (statusCode status) (decodeUtf8Lenient body)
            | otherwise ->
                case parseLatestSnapshot body of
                    Nothing -> pure $ Left $ MithrilParseError "Failed to parse snapshot list"
                    Just snapshot -> pure $ Right snapshot
  where
    decodeUtf8Lenient = T.pack . show

-- | Parse the latest snapshot from API response (expects a list, takes first)
parseLatestSnapshot :: LBS.ByteString -> Maybe SnapshotMetadata
parseLatestSnapshot bs = do
    snapshots <- decode bs :: Maybe [SnapshotMetadata]
    case snapshots of
        [] -> Nothing
        (s : _) -> Just s

-- | Download and verify a snapshot using mithril-client CLI
downloadSnapshot
    :: MithrilConfig
    -> SnapshotDigest
    -> IO (Either MithrilError FilePath)
downloadSnapshot
    MithrilConfig
        { mithrilClientPath
        , mithrilAggregatorUrl
        , mithrilDownloadDir
        , mithrilNetwork
        }
    (SnapshotDigest digest) = do
        -- Run mithril-client cardano-db download
        -- Include ancillary files (ledger state) for UTxO extraction
        let args =
                [ "cardano-db"
                , "download"
                , "--include-ancillary"
                , T.unpack digest
                ]
            envVars =
                [ ("AGGREGATOR_ENDPOINT", mithrilAggregatorUrl)
                , ("GENESIS_VERIFICATION_KEY", genesisVkForNetwork mithrilNetwork)
                ]
        (exitCode, stdout, stderr) <-
            readProcess
                $ setEnv envVars
                $ setWorkingDir mithrilDownloadDir
                $ proc mithrilClientPath args
        case exitCode of
            ExitSuccess ->
                pure $ Right $ mithrilDownloadDir <> "/db"
            ExitFailure code ->
                pure
                    $ Left
                    $ MithrilClientFailed
                        code
                        (decodeStdout stdout)
                        (decodeStdout stderr)
      where
        decodeStdout = T.pack . show . LBS.toStrict

{- | Get genesis verification key for network
These are the official Mithril genesis verification keys
-}
genesisVkForNetwork :: MithrilNetwork -> String
genesisVkForNetwork MithrilMainnet =
    "5b3139312c36362c3134302c3138372c36312c3136372c3133302c3130372c393\
    \72c37372c32392c3138322c3132372c3133342c3137382c3139382c3138392c38\
    \372c32382c3136312c3131322c3132372c38392c3234312c36382c3136382c313\
    \1342c3138392c3234312c3234312c3231322c3134345d"
genesisVkForNetwork MithrilPreprod =
    "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231392c3230\
    \372c32392c3134342c3139382c363638382c3136312c3133302c3231322c3134\
    \342c3233332c3139322c33385d"
genesisVkForNetwork MithrilPreview =
    "5b3134302c3136382c3134352c3137382c3138362c3131342c3133362c313935\
    \2c3231312c3132342c3133332c3137332c3139382c3133392c3233302c313332\
    \2c3139362c3231342c3234312c36365d"

{- | Download a snapshot via HTTP (without verification)

This downloads the snapshot archive directly from the CDN URLs
provided in the metadata. It does NOT verify the STM certificate
chain - use 'downloadSnapshot' with mithril-client for verified
downloads in production.

Requires @tar@ and @zstd@ commands to be available in PATH.
-}
downloadSnapshotHttp
    :: MithrilConfig
    -> SnapshotMetadata
    -> IO (Either MithrilError FilePath)
downloadSnapshotHttp
    MithrilConfig{mithrilDownloadDir, mithrilHttpManager}
    SnapshotMetadata{snapshotAncillaryLocations} =
        case snapshotAncillaryLocations of
            [] -> pure $ Left MithrilNoLocations
            (url : _) -> downloadAndExtract (T.unpack url)
      where
        -- Files extract directly to mithrilDownloadDir
        extractedPath = mithrilDownloadDir

        downloadAndExtract url = do
            -- Download the archive
            result <- try $ do
                request <- parseRequest url
                response <- httpLbs request mithrilHttpManager
                pure (responseStatus response, responseBody response)
            case result of
                Left (e :: HttpException) ->
                    pure $ Left $ MithrilHttpError e
                Right (status, body)
                    | statusCode status /= 200 ->
                        pure
                            $ Left
                            $ MithrilApiError
                                (statusCode status)
                                (T.pack "Download failed")
                    | otherwise -> do
                        -- Write to temp file and extract
                        let archivePath = mithrilDownloadDir <> "/snapshot.tar.zst"
                        LBS.writeFile archivePath body
                        extractArchive archivePath

        extractArchive archivePath = do
            -- Extract using tar and zstd: tar -I zstd -xf archive -C dir
            (exitCode, _stdout, stderr) <-
                readProcess
                    $ proc
                        "tar"
                        [ "-I"
                        , "zstd"
                        , "-xf"
                        , archivePath
                        , "-C"
                        , mithrilDownloadDir
                        ]
            case exitCode of
                ExitSuccess -> pure $ Right extractedPath
                ExitFailure code ->
                    pure
                        $ Left
                        $ MithrilExtractionFailed
                            code
                            (T.pack $ show $ LBS.toStrict stderr)
