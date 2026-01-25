{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Cardano.UTxOCSMT.Mithril.Extraction
Description : UTxO extraction from Mithril ledger state snapshots

This module provides functionality to extract UTxOs from Cardano ledger
state files downloaded via Mithril. It supports:

* Locating ledger state files in the ledger/ directory
* Streaming UTxOs from the InMemory backing store format

The extraction reads the raw CBOR bytes from the tables/tvar file
which stores UTxOs in the InMemory backing store format.
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

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read (DeserialiseFailure)
import Codec.CBOR.Read qualified as CBOR
import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
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
    | -- | Tables file not found
      TablesNotFound FilePath
    | -- | Failed to decode tables
      TablesDecodeFailed DeserialiseFailure
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
renderExtractionError (TablesNotFound path) =
    "UTxO tables file not found: " <> path
renderExtractionError (TablesDecodeFailed err) =
    "Failed to decode UTxO tables: " <> show err

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

{- | Find the latest ledger state in the ledger/ directory

Supports two formats:
1. Old format: @\<slot\>-\<hash\>.lstate@ files in db\/ledger\/
2. New UTxO-HD format: @\<slot\>\/@ directories in ledger\/

This function finds the entry with the highest slot number.
-}
findLedgerStateFile
    :: FilePath
    -- ^ Path to snapshot directory (e.g., from Mithril download)
    -> IO (Either ExtractionError (FilePath, Word64))
findLedgerStateFile snapshotPath = do
    -- Try new format first: ledger/<slot>/ directories
    let newLedgerDir = snapshotPath </> "ledger"
    newExists <- doesDirectoryExist newLedgerDir
    if newExists
        then findNewFormat newLedgerDir
        else do
            -- Try old format: db/ledger/<slot>-<hash>.lstate
            let oldLedgerDir = snapshotPath </> "db" </> "ledger"
            oldExists <- doesDirectoryExist oldLedgerDir
            if oldExists
                then findOldFormat oldLedgerDir
                else pure $ Left $ LedgerStateNotFound snapshotPath
  where
    -- New UTxO-HD format: directories named by slot number
    findNewFormat ledgerDir = do
        entries <- listDirectory ledgerDir
        let parsedEntries =
                [(e, s) | e <- entries, Just s <- [readMaybe e]]
        case sortOn (Down . snd) parsedEntries of
            [] -> pure $ Left $ NoLedgerStateFiles ledgerDir
            ((dir, slot) : _) ->
                pure $ Right (ledgerDir </> dir, slot)

    -- Old format: <slot>-<hash>.lstate files
    findOldFormat ledgerDir = do
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
1. Locates the ledger state directory in the snapshot
2. Reads the InMemory tables file (tables/tvar)
3. Streams UTxOs as raw CBOR-encoded (TxIn, TxOut) byte pairs

The tables/tvar file format is:
- WithOrigin SlotNo (CBOR)
- List of length 1 containing the map
- Map of key-value pairs (CBOR bytes for each)

The streaming approach allows processing large UTxO sets without
loading everything into memory at once.
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
extractUTxOsFromSnapshot tracer dbPath consumer = do
    result <- try $ do
        -- Find the ledger state file
        findResult <- findLedgerStateFile dbPath
        case findResult of
            Left err -> pure $ Left err
            Right (filePath, slot) -> do
                traceWith tracer $ ExtractionFoundLedgerState filePath slot

                -- Check for InMemory tables file
                let tablesFile = filePath </> "tables" </> "tvar"
                tablesExists <- doesFileExist tablesFile

                if not tablesExists
                    then pure $ Left $ TablesNotFound tablesFile
                    else do
                        traceWith tracer $ ExtractionStarting tablesFile

                        -- Stream UTxOs from tables/tvar
                        streamResult <- streamUtxos tracer tablesFile consumer

                        case streamResult of
                            Left err -> pure $ Left err
                            Right a -> do
                                traceWith tracer $ ExtractionComplete slot
                                pure $ Right a
    case result of
        Left (e :: IOException) -> pure $ Left $ ExtractionIOError e
        Right r -> pure r

-- | Stream UTxOs from the tables/tvar file
streamUtxos
    :: Tracer IO ExtractionTrace
    -> FilePath
    -- ^ Path to tvar file
    -> (Stream (Of (ByteString, ByteString)) IO () -> IO a)
    -- ^ Consumer
    -> IO (Either ExtractionError a)
streamUtxos tracer tvarPath consumer = do
    -- Read the file content
    content <- LBS.readFile tvarPath

    -- Parse the header and get the remaining bytes with the map
    case parseHeader content of
        Left err -> pure $ Left $ TablesDecodeFailed err
        Right (remaining, mapLen) -> do
            -- Create a stream of UTxO key-value pairs
            let stream' = streamKVPairs mapLen remaining
                trackedStream = trackProgress tracer 0 stream'
            a <- consumer trackedStream
            pure $ Right a

{- | Parse the tvar file header and return remaining bytes + map length

The tvar file format (Mithril snapshot) is:
1. List of length 1 (the tables wrapper)
2. Map (indefinite-length in practice)
3. Key-value pairs as CBOR bytes
-}
parseHeader
    :: ByteString -> Either DeserialiseFailure (ByteString, Maybe Int)
parseHeader = CBOR.deserialiseFromBytes headerDecoder
  where
    headerDecoder :: CBOR.Decoder s (Maybe Int)
    headerDecoder = do
        -- Decode list wrapper (length 1)
        _ <- CBOR.decodeListLen
        -- Decode map length (Nothing = indefinite)
        CBOR.decodeMapLenOrIndef

-- | Stream key-value pairs from the remaining bytes
streamKVPairs
    :: Maybe Int
    -- ^ Map length (Nothing = indefinite)
    -> ByteString
    -- ^ Remaining bytes after header
    -> Stream (Of (ByteString, ByteString)) IO ()
streamKVPairs mLen content = case mLen of
    Nothing -> streamIndefinite content
    Just n -> streamFixed n content
  where
    -- Decode a single key-value pair (both are CBOR bytes)
    kvDecoder :: CBOR.Decoder s (ByteString, ByteString)
    kvDecoder = do
        k <- LBS.fromStrict <$> CBOR.decodeBytes
        v <- LBS.fromStrict <$> CBOR.decodeBytes
        pure (k, v)

    streamFixed
        :: Int -> ByteString -> Stream (Of (ByteString, ByteString)) IO ()
    streamFixed 0 _ = pure ()
    streamFixed n bs =
        case CBOR.deserialiseFromBytes kvDecoder bs of
            Left _ -> pure ()
            Right (rest, kv) -> do
                S.yield kv
                streamFixed (n - 1) rest

    streamIndefinite
        :: ByteString -> Stream (Of (ByteString, ByteString)) IO ()
    streamIndefinite bs =
        case CBOR.deserialiseFromBytes breakOrKv bs of
            Left _ -> pure ()
            Right (_, Nothing) -> pure () -- Break marker
            Right (rest, Just kv) -> do
                S.yield kv
                streamIndefinite rest

    breakOrKv :: CBOR.Decoder s (Maybe (ByteString, ByteString))
    breakOrKv = do
        isBreak <- CBOR.decodeBreakOr
        if isBreak
            then pure Nothing
            else Just <$> kvDecoder

-- | Track progress and trace every 10000 UTxOs
trackProgress
    :: Tracer IO ExtractionTrace
    -> Word64
    -> Stream (Of (ByteString, ByteString)) IO ()
    -> Stream (Of (ByteString, ByteString)) IO ()
trackProgress tracer = go
  where
    go !count stream' = do
        next <- lift $ S.next stream'
        case next of
            Left () -> pure ()
            Right (item, rest) -> do
                S.yield item
                let newCount = count + 1
                when (newCount `mod` 10000 == 0)
                    $ lift
                    $ traceWith tracer
                    $ ExtractionProgress newCount
                go newCount rest
