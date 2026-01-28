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

import Cardano.UTxOCSMT.Mithril.AncillaryVerifier
    ( AncillaryVerificationError
    , AncillaryVerificationKey
    , verifyAncillaryManifest
    )
import Control.Exception (Exception, try)
import Control.Monad (unless)
import Control.Tracer (Tracer, traceWith)
import Data.Aeson
    ( FromJSON (..)
    , decode
    , withObject
    , (.:)
    , (.:?)
    )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Network.HTTP.Client
    ( BodyReader
    , HttpException
    , Manager
    , brRead
    , httpLbs
    , parseRequest
    , responseBody
    , responseStatus
    , withResponse
    )
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (..), withBinaryFile)
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
    , mithrilGenesisVk :: Maybe String
    -- ^ Genesis verification key for mithril-client CLI (JSON-hex format)
    , mithrilClientPath :: FilePath
    -- ^ Path to mithril-client binary
    , mithrilDownloadDir :: FilePath
    -- ^ Directory for downloading snapshots
    , mithrilHttpManager :: Manager
    -- ^ HTTP manager for API requests
    , mithrilAncillaryVk :: Maybe AncillaryVerificationKey
    -- ^ Optional Ed25519 verification key for ancillary files
    }

-- | Create default configuration for a network
defaultMithrilConfig
    :: Manager -> MithrilNetwork -> FilePath -> MithrilConfig
defaultMithrilConfig manager network downloadDir =
    MithrilConfig
        { mithrilNetwork = network
        , mithrilAggregatorUrl = defaultAggregatorUrl network
        , mithrilGenesisVk = Nothing
        , mithrilClientPath = "mithril-client"
        , mithrilDownloadDir = downloadDir
        , mithrilHttpManager = manager
        , mithrilAncillaryVk = Nothing
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
    | -- | Ed25519 ancillary verification failed
      MithrilVerificationFailed AncillaryVerificationError
    | -- | Missing genesis verification key for mithril-client CLI
      MithrilMissingGenesisVk
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
renderMithrilError (MithrilVerificationFailed err) =
    "Ancillary verification failed: " <> show err
renderMithrilError MithrilMissingGenesisVk =
    "Missing GENESIS_VERIFICATION_KEY for mithril-client CLI"

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
    | -- | Verifying ancillary Ed25519 signature
      MithrilVerifyingAncillary FilePath
    | -- | Ancillary verification successful
      MithrilAncillaryVerified
    | -- | Skipping ancillary verification (no key configured)
      MithrilAncillarySkipped
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
renderMithrilTrace (MithrilVerifyingAncillary path) =
    "Verifying ancillary Ed25519 signature at: " <> path
renderMithrilTrace MithrilAncillaryVerified =
    "Ancillary Ed25519 verification successful"
renderMithrilTrace MithrilAncillarySkipped =
    "Skipping ancillary verification (no key configured for network)"

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

{- | Download and verify a snapshot using mithril-client CLI

Uses v2 backend with minimal range (--start N --end N) to download only
the last immutable file plus ancillary files. This reduces download size
from ~14GB to ~500MB-1GB while maintaining full STM verification.
-}
downloadSnapshot
    :: MithrilConfig
    -> SnapshotMetadata
    -> IO (Either MithrilError FilePath)
downloadSnapshot
    MithrilConfig
        { mithrilClientPath
        , mithrilAggregatorUrl
        , mithrilDownloadDir
        , mithrilGenesisVk
        , mithrilNetwork
        }
    SnapshotMetadata{snapshotDigest, snapshotBeaconSlot} =
        case mithrilGenesisVk of
            Nothing -> pure $ Left MithrilMissingGenesisVk
            Just genesisVk -> do
                -- Run mithril-client cardano-db download with v2 backend
                -- Use --start and --end to download only the last immutable
                -- file plus ancillary files (ledger state) for UTxO extraction
                -- This reduces download from ~14GB to ~500MB-1GB
                currentEnv <- getEnvironment
                let immutableNum = show snapshotBeaconSlot
                    args =
                        [ "cardano-db"
                        , "download"
                        , "--backend"
                        , "v2"
                        , "--start"
                        , immutableNum
                        , "--end"
                        , immutableNum
                        , "--include-ancillary"
                        , "--download-dir"
                        , mithrilDownloadDir
                        , T.unpack (unSnapshotDigest snapshotDigest)
                        ]
                    -- Merge custom env vars with current environment
                    customEnvVars =
                        [ ("AGGREGATOR_ENDPOINT", mithrilAggregatorUrl)
                        , ("GENESIS_VERIFICATION_KEY", genesisVk)
                        ,
                            ( "ANCILLARY_VERIFICATION_KEY"
                            , ancillaryVkForNetwork mithrilNetwork
                            )
                        ]
                    mergedEnv =
                        customEnvVars
                            ++ filter
                                ((`notElem` map fst customEnvVars) . fst)
                                currentEnv
                (exitCode, stdout, stderr) <-
                    readProcess
                        $ setEnv mergedEnv
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

{- | Download a snapshot via HTTP (without verification)

This downloads the snapshot archive directly from the CDN URLs
provided in the metadata. It does NOT verify the STM certificate
chain - use 'downloadSnapshot' with mithril-client for verified
downloads in production.

Requires @tar@ and @zstd@ commands to be available in PATH.
-}
downloadSnapshotHttp
    :: Tracer IO MithrilTrace
    -> MithrilConfig
    -> SnapshotMetadata
    -> IO (Either MithrilError FilePath)
downloadSnapshotHttp
    tracer
    MithrilConfig
        { mithrilDownloadDir
        , mithrilHttpManager
        , mithrilAncillaryVk
        }
    SnapshotMetadata{snapshotAncillaryLocations} =
        case snapshotAncillaryLocations of
            [] -> pure $ Left MithrilNoLocations
            (url : _) -> downloadAndExtract (T.unpack url)
      where
        -- Files extract directly to mithrilDownloadDir
        extractedPath = mithrilDownloadDir

        downloadAndExtract url = do
            -- Download the archive using streaming to avoid loading
            -- the entire response into memory
            result <- try $ do
                request <- parseRequest url
                withResponse request mithrilHttpManager $ \response -> do
                    let status = responseStatus response
                    if statusCode status /= 200
                        then pure $ Left (statusCode status)
                        else do
                            -- Ensure download directory exists
                            createDirectoryIfMissing True mithrilDownloadDir
                            let archivePath =
                                    mithrilDownloadDir <> "/snapshot.tar.zst"
                            withBinaryFile archivePath WriteMode $ \handle ->
                                streamBodyToFile handle (responseBody response)
                            pure $ Right ()
            case result of
                Left (e :: HttpException) ->
                    pure $ Left $ MithrilHttpError e
                Right (Left code) ->
                    pure
                        $ Left
                        $ MithrilApiError code (T.pack "Download failed")
                Right (Right ()) -> do
                    let archivePath = mithrilDownloadDir <> "/snapshot.tar.zst"
                    extractResult <- extractArchive archivePath
                    case extractResult of
                        Left err -> pure $ Left err
                        Right path -> verifyIfConfigured path

        -- Stream response body to file handle chunk by chunk
        streamBodyToFile :: Handle -> BodyReader -> IO ()
        streamBodyToFile handle bodyReader = loop
          where
            loop = do
                chunk <- brRead bodyReader
                unless (BS.null chunk) $ do
                    BS.hPut handle chunk
                    loop

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

        -- Verify ancillary manifest if verification key is configured
        verifyIfConfigured path = case mithrilAncillaryVk of
            Nothing -> do
                -- No verification key configured, skip verification
                traceWith tracer MithrilAncillarySkipped
                pure $ Right path
            Just vk -> do
                traceWith tracer $ MithrilVerifyingAncillary path
                verifyResult <- verifyAncillaryManifest vk path
                case verifyResult of
                    Left err ->
                        pure $ Left $ MithrilVerificationFailed err
                    Right () -> do
                        traceWith tracer MithrilAncillaryVerified
                        pure $ Right path

{- | Get ancillary verification key for network (JSON-hex format)

These are the official Mithril ancillary verification keys for signing
volatile and ledger state files. Used by mithril-client for v2 backend.
-}
ancillaryVkForNetwork :: MithrilNetwork -> String
ancillaryVkForNetwork MithrilMainnet =
    "5b32332c37312c39362c3133332c34372c3235332c3232362c3133362c323335\
    \2c35372c3136342c3130362c3138362c322c32312c32392c3132302c3136332c\
    \38392c3132312c3137372c3133382c3230382c3133382c3231342c39392c3538\
    \2c32322c302c35382c332c36395d"
ancillaryVkForNetwork MithrilPreprod =
    "5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c323130\
    \2c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c\
    \3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c3134\
    \2c3232382c3136382c3132392c38332c3133362c33365d"
ancillaryVkForNetwork MithrilPreview =
    "5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c323130\
    \2c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c\
    \3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c3134\
    \2c3232382c3136382c3132392c38332c3133362c33365d"
