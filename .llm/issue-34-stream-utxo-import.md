# Issue #34: Stream UTxOs from Disk to Database During Import

## Problem

The Mithril extraction module (`Cardano.UTxOCSMT.Mithril.Extraction`) was
loading the entire `tables/tvar` file into memory using `LBS.readFile` before
processing. For mainnet snapshots with ~40M UTxOs, this consumed excessive
memory.

**Problem location**: `lib/Cardano/UTxOCSMT/Mithril/Extraction.hs:267`
```haskell
streamUtxos tracer tvarPath consumer = do
    -- Read the file content
    content <- LBS.readFile tvarPath  -- THIS LOADS ENTIRE FILE INTO MEMORY
    ...
```

## Solution

Refactored `streamUtxos` to use true incremental streaming:

1. **File handle streaming**: Instead of `LBS.readFile`, open file with
   `withFile` and stream bytes using `Streaming.ByteString.hGetContents`

2. **Incremental CBOR parsing**: Use `cborg`'s `deserialiseIncremental` API
   to parse CBOR incrementally as chunks arrive, following the pattern from
   `Bootstrap/App.hs`

3. **Lazy memory usage**: Only keep the current chunk and decoder state in
   memory, not the entire file contents

## Key Changes

### New Imports
```haskell
import Codec.CBOR.Read (IDecode (..), deserialiseIncremental)
import Control.Monad.ST (RealWorld, stToIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Streaming.ByteString qualified as SB
import System.IO (IOMode (..), withFile)
```

### Streaming Pipeline

```
File Handle
    |
    v
SB.hGetContents (streaming bytes)
    |
    v
SB.toChunks (strict ByteString chunks)
    |
    v
parseHeader (incremental CBOR)
    |
    v
streamKVPairs (incremental CBOR)
    |
    v
trackProgress
    |
    v
Consumer
```

### Key Functions

1. **`parseHeader`**: Incrementally parses the tvar file header (list wrapper
   + map length). Returns remaining byte stream after header.

2. **`streamKVPairs`**: Incrementally decodes CBOR key-value pairs. Handles
   both fixed-length and indefinite-length maps (with break marker).

3. **`feedFixed`/`feedIndefinite`**: Feed byte chunks to the incremental
   decoder and yield decoded KV pairs.

## Type Changes

The consumer type changed from:
```haskell
Stream (Of (ByteString, ByteString)) IO ()  -- strict ByteString
```
to:
```haskell
Stream (Of (LazyByteString, LazyByteString)) IO ()  -- lazy ByteString
```

This is compatible with `Streaming.hs` which already imports lazy ByteString
from `Data.ByteString.Lazy`.

## Testing

- All 38 unit tests pass
- Build succeeds for all packages
- Import functionality tested via `ImportSpec.hs`

## Memory Characteristics

**Before**: O(file_size) - entire file loaded into memory
**After**: O(chunk_size + decoder_state) - only current chunk and decoder
           state in memory

The default chunk size from `streaming-bytestring` is typically 32KB, so
memory usage is now bounded regardless of file size.

## Reference

The streaming pattern follows `Bootstrap/App.hs` which demonstrates proper
incremental CBOR parsing with the `streaming` library.
