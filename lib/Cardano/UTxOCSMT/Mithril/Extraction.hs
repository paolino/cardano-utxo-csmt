{- |
Module      : Cardano.UTxOCSMT.Mithril.Extraction
Description : UTxO extraction from Mithril ledger state snapshots

This module provides functionality to extract UTxOs from Cardano ledger
state files downloaded via Mithril. It supports:

* Locating ledger state files in the db/ledger/ directory
* Decoding the ExtLedgerState using ouroboros-consensus-cardano
* Extracting UTxOs across all post-Byron eras
* Streaming UTxOs as CBOR-encoded key-value pairs

The extraction uses the same CBOR encoding format as the chain sync
module for consistency in the CSMT database.

Note: Full ledger state decoding requires compatibility with the current
cardano-node ledger format, which includes UTxO-HD support. The decoding
implementation is marked as TODO pending stabilization of the UTxO-HD API.
-}
module Cardano.UTxOCSMT.Mithril.Extraction
    ( -- * Extraction
      extractUTxOsFromSnapshot
    , findLedgerStateFile

      -- * Results
    , ExtractionResult (..)

      -- * Errors
    , ExtractionError (..)
    , renderExtractionError

      -- * Tracing
    , ExtractionTrace (..)
    , renderExtractionTrace
    )
where

import Codec.CBOR.Read (DeserialiseFailure)
import Control.Exception (IOException, try)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Streaming (Of, Stream)
import System.Directory
    ( doesDirectoryExist
    , listDirectory
    )
import System.FilePath (takeExtension, (</>))

-- | Result of UTxO extraction
data ExtractionResult = ExtractionResult
    { extractedCount :: Word64
    -- ^ Number of UTxOs extracted
    , ledgerStateSlot :: Word64
    -- ^ Slot number of the ledger state
    }
    deriving stock (Show, Eq)

-- | Errors during UTxO extraction
data ExtractionError
    = -- | Ledger state directory not found
      LedgerStateNotFound FilePath
    | -- | No ledger state files in directory
      NoLedgerStateFiles FilePath
    | -- | Failed to decode ledger state file
      LedgerStateDecodeFailed FilePath Text DeserialiseFailure
    | -- | Unsupported ledger era (e.g., Byron)
      UnsupportedLedgerEra Text
    | -- | IO error during extraction
      ExtractionIOError IOException
    deriving stock (Show)

-- | Render extraction error for logging
renderExtractionError :: ExtractionError -> String
renderExtractionError (LedgerStateNotFound path) =
    "Ledger state directory not found: " <> path
renderExtractionError (NoLedgerStateFiles path) =
    "No ledger state files found in: " <> path
renderExtractionError (LedgerStateDecodeFailed path ctx err) =
    "Failed to decode ledger state file "
        <> path
        <> " ("
        <> T.unpack ctx
        <> "): "
        <> show err
renderExtractionError (UnsupportedLedgerEra era) =
    "Unsupported ledger era: " <> T.unpack era
renderExtractionError (ExtractionIOError e) =
    "IO error during extraction: " <> show e

-- | Trace events during extraction
data ExtractionTrace
    = -- | Starting extraction from ledger state file
      ExtractionStarting FilePath
    | -- | Found ledger state file at slot
      ExtractionFoundLedgerState FilePath Word64
    | -- | Decoding ledger state
      ExtractionDecoding
    | -- | Decoded successfully, extracting UTxOs
      ExtractionDecodedState Word64
    | -- | Progress: UTxOs extracted so far
      ExtractionProgress Word64
    | -- | Extraction complete
      ExtractionComplete Word64
    deriving stock (Show, Eq)

-- | Render extraction trace for logging
renderExtractionTrace :: ExtractionTrace -> String
renderExtractionTrace (ExtractionStarting path) =
    "Starting UTxO extraction from: " <> path
renderExtractionTrace (ExtractionFoundLedgerState path slot) =
    "Found ledger state at slot " <> show slot <> ": " <> path
renderExtractionTrace ExtractionDecoding =
    "Decoding ledger state file..."
renderExtractionTrace (ExtractionDecodedState count) =
    "Decoded ledger state with " <> show count <> " UTxOs"
renderExtractionTrace (ExtractionProgress count) =
    "Extracted " <> show count <> " UTxOs..."
renderExtractionTrace (ExtractionComplete count) =
    "Extraction complete: " <> show count <> " UTxOs"

{- | Find the latest ledger state file in the db/ledger/ directory

Ledger state files follow the naming pattern: @\<slot\>-\<hash\>.lstate@
This function finds the file with the highest slot number.
-}
findLedgerStateFile
    :: FilePath
    -- ^ Path to db directory (e.g., from Mithril download)
    -> IO (Either ExtractionError (FilePath, Word64))
findLedgerStateFile dbPath = do
    let ledgerDir = dbPath </> "ledger"
    exists <- doesDirectoryExist ledgerDir
    if not exists
        then pure $ Left $ LedgerStateNotFound ledgerDir
        else do
            files <- listDirectory ledgerDir
            let lstateFiles =
                    filter (\f -> takeExtension f == ".lstate") files
                parsedFiles = parseSlotFromFilename <$> lstateFiles
                validFiles =
                    [(f, s) | (f, Just s) <- zip lstateFiles parsedFiles]
            case sortOn (Down . snd) validFiles of
                [] -> pure $ Left $ NoLedgerStateFiles ledgerDir
                ((file, slot) : _) ->
                    pure $ Right (ledgerDir </> file, slot)
  where
    parseSlotFromFilename :: FilePath -> Maybe Word64
    parseSlotFromFilename name =
        case T.splitOn "-" (T.pack $ takeWhile (/= '.') name) of
            (slotText : _) -> readMaybe $ T.unpack slotText
            _ -> Nothing

    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

{- | Extract UTxOs from a Mithril snapshot ledger state

This function:
1. Locates the ledger state file in the snapshot
2. Reads and decodes the ExtLedgerState
3. Extracts UTxOs from the current era's ledger state
4. Streams UTxOs as CBOR-encoded (TxIn, TxOut) pairs

The ledger state decoding uses ouroboros-consensus-cardano types.
The streaming approach allows processing large UTxO sets without
loading everything into memory at once.

Note: The current implementation returns an error indicating that
ledger state decoding requires additional work to support the
UTxO-HD API changes in recent cardano-node versions.
-}
extractUTxOsFromSnapshot
    :: Tracer IO ExtractionTrace
    -> FilePath
    -- ^ Path to db directory containing ledger/
    -> ( Stream (Of (ByteString, ByteString)) IO ()
         -> IO a
       )
    -- ^ Consumer for the UTxO stream
    -> IO (Either ExtractionError a)
extractUTxOsFromSnapshot tracer dbPath _consumer = do
    result <- try $ do
        -- Find the ledger state file
        findResult <- findLedgerStateFile dbPath
        case findResult of
            Left err -> pure $ Left err
            Right (filePath, slot) -> do
                traceWith tracer $ ExtractionFoundLedgerState filePath slot
                traceWith tracer $ ExtractionStarting filePath

                -- TODO: Implement ledger state decoding
                -- The ExtLedgerState type now requires a MapKind parameter
                -- for UTxO-HD support. This needs to be addressed once
                -- the UTxO-HD API stabilizes.
                pure
                    $ Left
                    $ UnsupportedLedgerEra
                        "Ledger state decoding requires UTxO-HD API support"
    case result of
        Left (e :: IOException) -> pure $ Left $ ExtractionIOError e
        Right r -> pure r
