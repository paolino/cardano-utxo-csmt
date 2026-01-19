module Cardano.N2N.Client.Application.Run.RocksDB
    ( main
    )
where

import CSMT (FromKV (..))
import CSMT.Hashes
    ( Hash (..)
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
    )
import Cardano.N2N.Client.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , setup
    )
import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Update
    ( PartialHistory (..)
    , newFinality
    )
import Cardano.N2N.Client.Application.Database.RocksDB
    ( newRocksDBState
    )
import Cardano.N2N.Client.Application.Options
    ( Options (..)
    , optionsParser
    )
import Cardano.N2N.Client.Application.Run.Application (application)
import Cardano.N2N.Client.Ouroboros.Types
    ( Point
    )
import Control.Lens (Prism', lazy, prism', strict, view)
import Control.Monad (when)
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Short (fromShort)
import Data.ByteString.Short qualified as B
import Data.Maybe (isNothing)
import Data.Serialize
    ( getShortByteString
    , getWord32be
    , getWord64be
    , putShortByteString
    , putWord32be
    , putWord64be
    )
import Data.Serialize.Extra (evalGetM, evalPutM)
import Database.KV.Cursor (firstEntry)
import Database.KV.Transaction (iterating)
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (..)
    , DB
    , withDBCF
    )
import OptEnvConf (runParser)
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Point qualified as Network
import Paths_cardano_utxo_csmt (version)

withRocksDB
    :: FilePath
    -> (DB -> IO b)
    -> IO b
withRocksDB path = do
    withDBCF
        path
        config
        [ ("kv", config)
        , ("csmt", config)
        , ("rollbacks", config)
        ]

config :: Config
config =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 1
        , prefixLength = Nothing
        , bloomFilter = False
        }

main :: IO ()
main = do
    options@Options{dbPath} <-
        runParser
            version
            "Tracking cardano UTxOs in a CSMT in a rocksDB database"
            optionsParser
    withRocksDB dbPath $ \db -> do
        ((state, slots), runner) <-
            newRocksDBState db prisms context Partial slotHash armageddonParams
        setupDB runner
        application options state slots (mFinality runner)

setupDB
    :: RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> IO ()
setupDB runner = do
    new <- checkEmptyRollbacks runner
    when new $ setup runner armageddonParams

checkEmptyRollbacks
    :: RunCSMTTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> IO Bool
checkEmptyRollbacks (RunCSMTTransaction runCSMT) =
    runCSMT $ do
        mfe <- iterating RollbackPoints firstEntry
        return $ isNothing mfe

mFinality
    :: (Ord key, MonadFail m)
    => RunCSMTTransaction cf op Point hash key value m
    -> m (Maybe Point)
mFinality (RunCSMTTransaction runCSMT) = runCSMT $ newFinality isFinal
  where
    isFinal :: WithOrigin Point -> WithOrigin Point -> Bool
    isFinal tip finality = distance tip finality > 2160

distance :: WithOrigin Point -> WithOrigin Point -> SlotNo
distance Origin _ = SlotNo 0
distance (At (Network.Point Origin)) _ = error "distance: tip at Origin has no slot"
distance (At (Network.Point (At (Network.Block slotTip _)))) Origin =
    slotTip
distance
    (At (Network.Point (At (Network.Block slotTip _))))
    (At (Network.Point (At (Network.Block slotFinality _)))) =
        SlotNo (unSlotNo slotTip - unSlotNo slotFinality)
distance _ _ = error "distance: finality at Origin has no slot"

armageddonParams :: ArmageddonParams Hash
armageddonParams =
    ArmageddonParams
        { noHash = mkHash ""
        , armageddonBatchSize = 1000
        }

context :: CSMTContext Hash LazyByteString LazyByteString
context =
    CSMTContext
        { fromKV =
            FromKV
                { fromK = fromK fromKVHashes . view strict
                , fromV = fromV fromKVHashes . view strict
                }
        , hashing = hashHashing
        }

prisms :: Prisms Point Hash LazyByteString LazyByteString
prisms = Prisms{..}
  where
    slotP :: Prism' StrictByteString Point
    slotP = prism' encode decode
      where
        encode :: Point -> StrictByteString
        encode (Network.Point Origin) = ""
        encode (Network.Point (At (Network.Block (SlotNo slot) (OneEraHash h)))) = do
            evalPutM $ do
                putWord64be slot
                putWord32be (fromIntegral $ B.length h)
                putShortByteString h

        decode :: StrictByteString -> Maybe Point
        decode bs
            | bs == "" = Just $ Network.Point Origin
            | otherwise = flip evalGetM bs $ do
                slot <- SlotNo <$> getWord64be
                len <- fromIntegral <$> getWord32be
                h <- getShortByteString len
                return $ Network.Point (At (Network.Block slot (OneEraHash h)))

    hashP :: Prism' StrictByteString Hash
    hashP = isoHash

    keyP :: Prism' StrictByteString LazyByteString
    keyP = lazy

    valueP :: Prism' StrictByteString LazyByteString
    valueP = lazy

slotHash :: Point -> Hash
slotHash (Network.Point Origin) = error "slotHash: Origin has no hash"
slotHash (Network.Point (At (Network.Block _ (OneEraHash h)))) =
    Hash $ fromShort h
