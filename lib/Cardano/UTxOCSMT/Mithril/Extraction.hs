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

The streaming approach uses incremental CBOR parsing to avoid loading
the entire file into memory at once.
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
import Codec.CBOR.Read
    ( DeserialiseFailure
    , IDecode (..)
    , deserialiseIncremental
    )
import Control.Exception (IOException, try)
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Streaming (Of, Stream, effect)
import Streaming.ByteString qualified as SB
import Streaming.Prelude qualified as S
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import System.FilePath (takeExtension, (</>))
import System.IO (IOMode (..), withFile)

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
    | -- | Counting UTxOs in snapshot
      ExtractionCounting Word64
    | -- | Counted total UTxOs
      ExtractionDecodedState Word64
    | -- | Starting extraction stream
      ExtractionStreamStarting
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
renderExtractionTrace (ExtractionCounting count) =
    "Counting UTxOs in snapshot..."
        <> show count
        <> " UTxOs so far..."
renderExtractionTrace (ExtractionDecodedState count) =
    "Counted " <> show count <> " UTxOs in snapshot"
renderExtractionTrace ExtractionStreamStarting =
    "Starting extraction stream..."
renderExtractionTrace (ExtractionProgress count) =
    "Extracted " <> show count <> " UTxOs so far..."
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

Returns the consumer result along with the slot number from the ledger
state directory, which is needed for chain sync to skip block fetching
until reaching that slot.
-}
extractUTxOsFromSnapshot
    :: Tracer IO ExtractionTrace
    -> FilePath
    -- ^ Path to db directory containing ledger/
    -> ( Stream (Of (LazyByteString, LazyByteString)) IO ()
         -> IO a
       )
    -- ^ Consumer for the UTxO stream
    -> IO (Either ExtractionError (a, Word64))
    -- ^ Result with slot number for skip-until logic
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
                                pure $ Right (a, slot)
    case result of
        Left (e :: IOException) -> pure $ Left $ ExtractionIOError e
        Right r -> pure r

{- | Stream UTxOs from the tables/tvar file

Uses incremental CBOR parsing to stream UTxOs without loading the
entire file into memory. The file is read in chunks and parsed
incrementally using the cborg library's 'deserialiseIncremental'.
-}
streamUtxos
    :: Tracer IO ExtractionTrace
    -> FilePath
    -- ^ Path to tvar file
    -> ( Stream (Of (LazyByteString, LazyByteString)) IO ()
         -> IO a
       )
    -- ^ Consumer
    -> IO (Either ExtractionError a)
streamUtxos tracer tvarPath consumer =
    withFile tvarPath ReadMode $ \handle -> do
        let byteStream =
                SB.hGetContents handle
                    & SB.toChunks
        -- Parse header incrementally
        headerResult <- parseHeader byteStream
        case headerResult of
            Left err ->
                pure $ Left $ TablesDecodeFailed err
            Right (mLen, remainingStream) -> do
                -- Get total count (from header or by counting)
                total <- case mLen of
                    Just len -> pure (fromIntegral len)
                    Nothing -> do
                        -- Count UTxOs first (fast pass through file)
                        countUtxos tracer tvarPath
                traceWith tracer $ ExtractionDecodedState total
                traceWith tracer ExtractionStreamStarting
                -- Stream KV pairs incrementally
                let kvStream = streamKVPairs mLen remainingStream
                    trackedStream = trackExtractionProgress tracer 0 kvStream
                a <- consumer trackedStream
                pure $ Right a

-- | Count UTxOs in tvar file without fully decoding
countUtxos :: Tracer IO ExtractionTrace -> FilePath -> IO Word64
countUtxos tracer tvarPath =
    withFile tvarPath ReadMode $ \handle -> do
        let byteStream = SB.hGetContents handle & SB.toChunks
        headerResult <- parseHeader byteStream
        case headerResult of
            Left _ -> pure 0
            Right (mLen, remainingStream) ->
                case mLen of
                    Just len -> pure (fromIntegral len)
                    Nothing -> do
                        -- Count by streaming through with progress
                        counting tracer
                            $ streamKVPairs Nothing remainingStream

-- | Count stream elements with progress tracing
counting
    :: Tracer IO ExtractionTrace
    -> Stream (Of a) IO ()
    -> IO Word64
counting tracer =
    S.foldM_
        ( \count _ -> do
            let newCount = count + 1
            traceWith tracer $ ExtractionCounting newCount
            pure newCount
        )
        (pure 0)
        pure

{- | Parse the tvar file header incrementally

The tvar file format (Mithril snapshot) is:
1. List of length 1 (the tables wrapper)
2. Map (indefinite-length in practice)
3. Key-value pairs as CBOR bytes

Returns the map length and remaining byte stream.
-}
parseHeader
    :: Stream (Of ByteString) IO ()
    -> IO
        (Either DeserialiseFailure (Maybe Int, Stream (Of ByteString) IO ()))
parseHeader byteStream = do
    decoder <- stToIO $ deserialiseIncremental headerDecoder
    feedHeader decoder byteStream
  where
    headerDecoder :: CBOR.Decoder RealWorld (Maybe Int)
    headerDecoder = do
        -- Decode list wrapper (length 1)
        _ <- CBOR.decodeListLen
        -- Decode map length (Nothing = indefinite)
        CBOR.decodeMapLenOrIndef

    feedHeader
        :: IDecode RealWorld (Maybe Int)
        -> Stream (Of ByteString) IO ()
        -> IO
            ( Either
                DeserialiseFailure
                (Maybe Int, Stream (Of ByteString) IO ())
            )
    feedHeader decoder stream' = case decoder of
        Done leftover _ mLen -> do
            -- Header parsed, return remaining stream
            let remaining =
                    if BS.null leftover
                        then stream'
                        else S.yield leftover >> stream'
            pure $ Right (mLen, remaining)
        Fail _ _ err ->
            pure $ Left err
        Partial k -> do
            next <- S.next stream'
            case next of
                Left () -> do
                    -- End of input during header parsing
                    decoder' <- stToIO $ k Nothing
                    feedHeader decoder' (pure ())
                Right (chunk, rest) -> do
                    decoder' <- stToIO $ k (Just chunk)
                    feedHeader decoder' rest

{- | Stream key-value pairs incrementally from byte chunks

The KV pairs are CBOR-encoded as:
- Key: CBOR bytes (decodeBytes)
- Value: CBOR bytes (decodeBytes)

For indefinite-length maps, we watch for the break marker.
-}
streamKVPairs
    :: Maybe Int
    -- ^ Map length (Nothing = indefinite)
    -> Stream (Of ByteString) IO ()
    -- ^ Remaining byte chunks after header
    -> Stream (Of (LazyByteString, LazyByteString)) IO ()
streamKVPairs mLen byteStream = case mLen of
    Nothing -> streamIndefinite byteStream
    Just n -> streamFixed n byteStream
  where
    -- Decode a single key-value pair (both are CBOR bytes)
    kvDecoder :: CBOR.Decoder RealWorld (LazyByteString, LazyByteString)
    kvDecoder = do
        k <- LBS.fromStrict <$> CBOR.decodeBytes
        v <- LBS.fromStrict <$> CBOR.decodeBytes
        pure (k, v)

    -- Decoder that checks for break marker first
    breakOrKvDecoder
        :: CBOR.Decoder
            RealWorld
            (Maybe (LazyByteString, LazyByteString))
    breakOrKvDecoder = do
        isBreak <- CBOR.decodeBreakOr
        if isBreak
            then pure Nothing
            else Just <$> kvDecoder

    streamFixed
        :: Int
        -> Stream (Of ByteString) IO ()
        -> Stream (Of (LazyByteString, LazyByteString)) IO ()
    streamFixed 0 _ = pure ()
    streamFixed n stream' = effect $ do
        decoder <- stToIO $ deserialiseIncremental kvDecoder
        pure $ feedFixed n decoder stream'

    feedFixed
        :: Int
        -> IDecode RealWorld (LazyByteString, LazyByteString)
        -> Stream (Of ByteString) IO ()
        -> Stream (Of (LazyByteString, LazyByteString)) IO ()
    feedFixed _ (Fail{}) _ = pure ()
    feedFixed n (Done leftover _ kv) stream' = do
        S.yield kv
        let remaining =
                if BS.null leftover
                    then stream'
                    else S.yield leftover >> stream'
        streamFixed (n - 1) remaining
    feedFixed n (Partial k) stream' = effect $ do
        next <- S.next stream'
        case next of
            Left () -> do
                decoder' <- stToIO $ k Nothing
                pure $ feedFixed n decoder' (pure ())
            Right (chunk, rest) -> do
                decoder' <- stToIO $ k (Just chunk)
                pure $ feedFixed n decoder' rest

    streamIndefinite
        :: Stream (Of ByteString) IO ()
        -> Stream (Of (LazyByteString, LazyByteString)) IO ()
    streamIndefinite stream' = effect $ do
        decoder <- stToIO $ deserialiseIncremental breakOrKvDecoder
        pure $ feedIndefinite decoder stream'

    feedIndefinite
        :: IDecode
            RealWorld
            (Maybe (LazyByteString, LazyByteString))
        -> Stream (Of ByteString) IO ()
        -> Stream (Of (LazyByteString, LazyByteString)) IO ()
    feedIndefinite (Fail{}) _ = pure ()
    feedIndefinite (Done leftover _ mKv) stream' = case mKv of
        Nothing -> pure () -- Break marker - end of map
        Just kv -> do
            S.yield kv
            let remaining =
                    if BS.null leftover
                        then stream'
                        else S.yield leftover >> stream'
            streamIndefinite remaining
    feedIndefinite (Partial k) stream' = effect $ do
        next <- S.next stream'
        case next of
            Left () -> do
                decoder' <- stToIO $ k Nothing
                pure $ feedIndefinite decoder' (pure ())
            Right (chunk, rest) -> do
                decoder' <- stToIO $ k (Just chunk)
                pure $ feedIndefinite decoder' rest

-- | Track progress and trace every UTxO
trackExtractionProgress
    :: Tracer IO ExtractionTrace
    -> Word64
    -> Stream (Of (LazyByteString, LazyByteString)) IO ()
    -> Stream (Of (LazyByteString, LazyByteString)) IO ()
trackExtractionProgress tracer = go
  where
    go !count stream' = do
        next <- lift $ S.next stream'
        case next of
            Left () -> pure ()
            Right (item, rest) -> do
                S.yield item
                let newCount = count + 1
                lift $ traceWith tracer $ ExtractionProgress newCount
                go newCount rest
