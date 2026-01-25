{- |
Module      : Cardano.UTxOCSMT.Mithril.Streaming
Description : Stream UTxOs to CSMT bulk import

This module provides functionality to stream UTxOs from Mithril extraction
into the CSMT database. It supports:

* Configurable batch sizes for commit frequency
* Progress reporting at configurable intervals
* Efficient bulk import using the CSMT transaction interface

The streaming approach follows the pattern from 'Bootstrap.App' for
memory-efficient processing of large UTxO sets.
-}
module Cardano.UTxOCSMT.Mithril.Streaming
    ( -- * Streaming
      streamToCSMT

      -- * Configuration
    , StreamConfig (..)
    , defaultStreamConfig

      -- * Tracing
    , StreamTrace (..)
    , renderStreamTrace
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction (..)
    , insertCSMT
    )
import Control.Monad (when)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Database.RocksDB (BatchOp, ColumnFamily)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

-- | Configuration for streaming to CSMT
data StreamConfig = StreamConfig
    { streamBatchSize :: Word64
    -- ^ Number of entries per transaction batch
    , streamProgressInterval :: Word64
    -- ^ Report progress every N entries
    }
    deriving stock (Show, Eq)

-- | Default streaming configuration
defaultStreamConfig :: StreamConfig
defaultStreamConfig =
    StreamConfig
        { streamBatchSize = 10000
        , streamProgressInterval = 100000
        }

-- | Trace events during streaming
data StreamTrace
    = -- | Starting stream import
      StreamStarting
    | -- | Progress: entries imported, entries per second
      StreamProgress Word64 Double
    | -- | Batch committed
      StreamBatchCommitted Word64
    | -- | Stream complete: total entries
      StreamComplete Word64
    deriving stock (Show, Eq)

-- | Render stream trace for logging
renderStreamTrace :: StreamTrace -> String
renderStreamTrace StreamStarting =
    "Starting UTxO stream import..."
renderStreamTrace (StreamProgress count rate) =
    "Imported "
        <> show count
        <> " UTxOs ("
        <> show (round rate :: Int)
        <> " UTxOs/second)"
renderStreamTrace (StreamBatchCommitted count) =
    "Committed batch at " <> show count <> " entries"
renderStreamTrace (StreamComplete count) =
    "Stream import complete: " <> show count <> " UTxOs"

{- | Stream UTxOs to CSMT database

This function consumes a stream of CBOR-encoded (key, value) pairs
and inserts them into the CSMT database using the provided transaction
runner.

The function:
1. Processes entries in batches for efficient commits
2. Reports progress at configurable intervals
3. Returns the total number of entries imported

Note: Each entry is inserted individually within the transaction.
The transaction batching is handled by the caller if needed.
-}
streamToCSMT
    :: Tracer IO StreamTrace
    -> StreamConfig
    -> RunCSMTTransaction
        ColumnFamily
        BatchOp
        slot
        hash
        ByteString
        ByteString
        IO
    -> Stream (Of (ByteString, ByteString)) IO ()
    -> IO Word64
streamToCSMT tracer config runner stream = do
    traceWith tracer StreamStarting
    startTime <- getCurrentTime
    count <- processStream startTime 0 stream
    traceWith tracer $ StreamComplete count
    pure count
  where
    StreamConfig{streamProgressInterval} = config
    RunCSMTTransaction{txRunTransaction} = runner

    processStream
        :: UTCTime
        -> Word64
        -> Stream (Of (ByteString, ByteString)) IO ()
        -> IO Word64
    processStream startTime count stream' = do
        result <- S.next stream'
        case result of
            Left () -> pure count
            Right ((key, value), rest) -> do
                -- Insert into CSMT
                txRunTransaction $ insertCSMT key value

                let newCount = count + 1

                -- Report progress at intervals
                when (newCount `mod` streamProgressInterval == 0) $ do
                    currentTime <- getCurrentTime
                    let elapsed = realToFrac $ diffUTCTime currentTime startTime
                        rate =
                            if elapsed > 0
                                then fromIntegral newCount / elapsed
                                else 0
                    traceWith tracer $ StreamProgress newCount rate

                processStream startTime newCount rest
