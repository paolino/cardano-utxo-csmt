{- |
Module      : Bench.CSMT
Description : CSMT insertion benchmarks

Benchmarks for CSMT insertion performance.
Note: Includes DB setup time as RocksDB uses bracket pattern.
-}
module Bench.CSMT
    ( loadGoldenUtxos
    , runInsertBench
    )
where

import CSMT (FromKV (..))
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    , insertCSMT
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBCSMTTransaction
    )
import Codec.Serialise (deserialise)
import Control.Lens (Prism', lazy, prism', strict, view)
import Control.Monad (forM_)
import Control.Tracer (nullTracer)
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Database.RocksDB
    ( Config (..)
    , withDBCF
    )
import System.IO.Temp (withSystemTempDirectory)

-- | Load golden UTxOs from file
loadGoldenUtxos :: IO [(ByteString, ByteString)]
loadGoldenUtxos = do
    content <- LBS.readFile "test/assets/golden-utxos.cbor"
    pure $ deserialise content

-- | Run insertion benchmark
runInsertBench
    :: [(ByteString, ByteString)]
    -> IO ()
runInsertBench utxos =
    withSystemTempDirectory "csmt-bench" $ \tmpDir ->
        withDBCF
            tmpDir
            rocksConfig
            [ ("kv", rocksConfig)
            , ("csmt", rocksConfig)
            , ("rollbacks", rocksConfig)
            , ("config", rocksConfig)
            ]
            $ \db -> do
                RunCSMTTransaction{txRunTransaction} <-
                    newRunRocksDBCSMTTransaction
                        db
                        benchPrisms
                        benchContext
                setup
                    nullTracer
                    (RunCSMTTransaction txRunTransaction)
                    benchArmageddonParams
                forM_ utxos $ \(k, v) ->
                    txRunTransaction $ insertCSMT k v

-- | RocksDB configuration
rocksConfig :: Config
rocksConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Prisms for ByteString keys/values
benchPrisms :: Prisms () Hash ByteString ByteString
benchPrisms = Prisms{..}
  where
    slotP :: Prism' StrictByteString ()
    slotP = prism' (const "") (const $ Just ())

    hashP :: Prism' StrictByteString Hash
    hashP = isoHash

    keyP :: Prism' StrictByteString ByteString
    keyP = lazy

    valueP :: Prism' StrictByteString ByteString
    valueP = lazy

-- | CSMT context for hashing
benchContext :: CSMTContext Hash ByteString ByteString
benchContext =
    CSMTContext
        { fromKV =
            FromKV
                { isoK = strict . isoK fromKVHashes
                , fromV = fromV fromKVHashes . view strict
                , treePrefix = const []
                }
        , hashing = hashHashing
        }

-- | Armageddon params for setup
benchArmageddonParams :: ArmageddonParams Hash
benchArmageddonParams =
    ArmageddonParams
        { noHash = mkHash ""
        , armageddonBatchSize = 1000
        }
