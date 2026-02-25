module Cardano.UTxOCSMT.Config
    ( -- * RocksDB configuration
      withRocksDB
    , dbConfig
    , utxoColumnFamilies

      -- * CSMT context
    , context
    , prisms
    , slotHash
    , armageddonParams

      -- * Point encoding
    , encodePoint
    , decodePoint

      -- * Finality
    , mFinality
    , distance
    )
where

-- |
-- Module      : Cardano.UTxOCSMT.Config
-- Description : Cardano-specific RocksDB and CSMT configuration
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Reusable configuration for the UTxO CSMT database:
-- column family layout, serialization prisms, CSMT
-- hashing context, and armageddon parameters.
-- Extracted so downstream packages can share the DB
-- schema without depending on the full application
-- layer (HTTP, option parsing, etc.).

import CSMT (FromKV (..), Key)
import CSMT.Hashes
    ( Hash (..)
    , byteStringToKey
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
    )
import Cardano.Ledger.Address (unCompactAddr)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.Binary (decodeFull, natVersion)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (compactAddrTxOutL)
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( newFinality
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Lens
    ( Prism'
    , lazy
    , preview
    , prism'
    , review
    , strict
    , view
    , (^.)
    )
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Short (fromShort)
import Data.ByteString.Short qualified as B
import Data.Serialize
    ( getShortByteString
    , getWord32be
    , getWord64be
    , putShortByteString
    , putWord32be
    , putWord64be
    )
import Data.Serialize.Extra (evalGetM, evalPutM)
import Database.RocksDB
    ( Config (..)
    , DB
    , withDBCF
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..)
    )
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )
import Ouroboros.Network.Point qualified as Network

-- | Column family names for the UTxO CSMT database.
--
-- Four column families:
--
--   * @kv@ — Key-value store for UTxO data
--   * @csmt@ — Compact Sparse Merkle Tree nodes
--   * @rollbacks@ — Rollback points for chain reorgs
--   * @config@ — Application configuration data
utxoColumnFamilies :: [(String, Config)]
utxoColumnFamilies =
    [ ("kv", dbConfig)
    , ("csmt", dbConfig)
    , ("rollbacks", dbConfig)
    , ("config", dbConfig)
    ]

{- | Open a RocksDB database with the UTxO column
families.
-}
withRocksDB
    :: FilePath
    -> (DB -> IO b)
    -> IO b
withRocksDB path =
    withDBCF path dbConfig utxoColumnFamilies

{- | Default RocksDB configuration.

Creates the database if it doesn't exist, without
paranoid checks or bloom filters.
-}
dbConfig :: Config
dbConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

{- | Parameters for the armageddon (database reset)
operation.
-}
armageddonParams :: ArmageddonParams Hash
armageddonParams =
    ArmageddonParams
        { noHash = mkHash ""
        , armageddonBatchSize = 1000
        }

{- | CSMT context for hashing operations.

Configures how keys and values are converted to
hashable form. The tree key is prefixed with address
bytes for by-address queries.
-}
context
    :: CSMTContext Hash LazyByteString LazyByteString
context =
    CSMTContext
        { fromKV =
            FromKV
                { isoK =
                    strict . isoK fromKVHashes
                , fromV =
                    fromV fromKVHashes . view strict
                , treePrefix =
                    addressPrefix . view strict
                }
        , hashing = hashHashing
        }

-- | Extract address bytes from a CBOR-encoded
-- Conway TxOut, convert to tree key prefix.
addressPrefix :: StrictByteString -> Key
addressPrefix cborTxOut = byteStringToKey addressBytes
  where
    addressBytes =
        case decodeFull
            (natVersion @11)
            (view lazy cborTxOut) of
            Left e ->
                error
                    $ "addressPrefix: decode failed: "
                        <> show e
            Right (txOut :: BabbageTxOut ConwayEra) ->
                fromShort
                    $ unCompactAddr
                        (txOut ^. compactAddrTxOutL)

{- | Prisms for encoding and decoding database values.

Provides bidirectional transformations between
Haskell types and their binary representations:

  * @slotP@ — Encodes 'Point' as slot + block hash
  * @hashP@ — Encodes 'Hash' using the standard iso
  * @keyP@, @valueP@ — Identity for lazy bytestrings
-}
prisms
    :: Prisms
        Point
        Hash
        LazyByteString
        LazyByteString
prisms = Prisms{..}
  where
    slotP :: Prism' StrictByteString Point
    slotP = prism' encode decode
      where
        encode :: Point -> StrictByteString
        encode (Network.Point Origin) = ""
        encode
            ( Network.Point
                    ( At
                            ( Network.Block
                                    (SlotNo slot)
                                    (OneEraHash h)
                                )
                        )
                ) = do
                evalPutM $ do
                    putWord64be slot
                    putWord32be
                        (fromIntegral $ B.length h)
                    putShortByteString h

        decode
            :: StrictByteString -> Maybe Point
        decode bs
            | bs == "" =
                Just $ Network.Point Origin
            | otherwise = flip evalGetM bs $ do
                slot <- SlotNo <$> getWord64be
                len <-
                    fromIntegral <$> getWord32be
                h <- getShortByteString len
                return
                    $ Network.Point
                        ( At
                            ( Network.Block
                                slot
                                (OneEraHash h)
                            )
                        )

    hashP :: Prism' StrictByteString Hash
    hashP = isoHash

    keyP :: Prism' StrictByteString LazyByteString
    keyP = lazy

    valueP :: Prism' StrictByteString LazyByteString
    valueP = lazy

{- | Extract the block hash from a 'Point'.

Fails with an error if called on 'Origin'.
-}
slotHash :: Point -> Hash
slotHash (Network.Point Origin) =
    error "slotHash: Origin has no hash"
slotHash
    ( Network.Point
            (At (Network.Block _ (OneEraHash h)))
        ) = Hash $ fromShort h

-- | Encode a Point to ByteString for config storage.
encodePoint :: Point -> StrictByteString
encodePoint = review (slotP prisms)

-- | Decode a ByteString to Point for config storage.
decodePoint :: StrictByteString -> Maybe Point
decodePoint = preview (slotP prisms)

{- | Compute the finality point based on slot distance.

Returns the most recent point that is considered
final (more than 2160 slots behind the tip,
approximately 12 hours on Cardano mainnet).
-}
mFinality
    :: MonadFail m
    => RunCSMTTransaction
        cf
        op
        Point
        hash
        key
        value
        m
    -> m (Maybe Point)
mFinality (RunCSMTTransaction runCSMT) =
    runCSMT $ newFinality isFinal
  where
    isFinal
        :: WithOrigin Point
        -> WithOrigin Point
        -> Bool
    isFinal tip finality =
        distance tip finality > 2160

{- | Calculate the slot distance between two points.

Returns 0 if the tip is at Origin.
-}
distance
    :: WithOrigin Point
    -> WithOrigin Point
    -> SlotNo
distance Origin _ = SlotNo 0
distance (At (Network.Point Origin)) _ =
    error "distance: tip at Origin has no slot"
distance
    ( At
            ( Network.Point
                    (At (Network.Block slotTip _))
                )
        )
    Origin = slotTip
distance
    ( At
            ( Network.Point
                    (At (Network.Block slotTip _))
                )
        )
    ( At
            ( Network.Point
                    ( At
                            ( Network.Block
                                    slotFinality
                                    _
                                )
                        )
                )
        ) =
        SlotNo
            (unSlotNo slotTip - unSlotNo slotFinality)
distance _ _ =
    error "distance: finality at Origin has no slot"
