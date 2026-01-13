module Cardano.N2N.Client.Application.Database.RocksDB.Structure
    ( Structure (..)
    , RollBackPoint (..)
    , RollbackPointKV
    , forwardTipApplyRocksDB
    , updateRollbackPoint
    )
where

import CSMT (FromKV, Hashing, inserting)
import CSMT.Deletion (deleting)
import CSMT.Interface (Indirect, Key)
import Cardano.N2N.Client.Application.Database.Interface
    ( Operation (..)
    )
import Control.Lens ((:~:) (..))
import Control.Monad (forM)
import Database.KV.Transaction
    ( GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    , Transaction
    , insert
    , query
    )
import Database.RocksDB

-- | Represents a rollback point in the database
data RollBackPoint slot hash key value = RollBackPoint
    { rbpHash :: hash
    , rbpInverseOperations :: [Operation key value]
    }

-- | Type alias for the RocksDB KV column storing rollback points
type RollbackPointKV slot hash key value =
    KV slot (RollBackPoint slot hash key value)

-- | Structure of the RocksDB database used by this application
data Structure slot hash key value x where
    KVCol :: Structure slot hash key value (KV key value)
        -- ^ Key-Value column for utxos
    CSMTCol :: Structure slot hash key value (KV Key (Indirect hash))
        -- ^ CSMT column for storing the CSMT of the UTxO set
    RollbackPoints
        :: Structure slot hash key value (RollbackPointKV slot hash key value)
        -- ^ Column for storing rollback points

instance GEq (Structure slot hash key value) where
    geq KVCol KVCol = Just Refl
    geq CSMTCol CSMTCol = Just Refl
    geq RollbackPoints RollbackPoints = Just Refl
    geq _ _ = Nothing

instance GCompare (Structure slot hash key value) where
    gcompare KVCol KVCol = GEQ
    gcompare KVCol _ = GLT
    gcompare _ KVCol = GGT
    gcompare CSMTCol CSMTCol = GEQ
    gcompare CSMTCol RollbackPoints = GLT
    gcompare RollbackPoints CSMTCol = GGT
    gcompare RollbackPoints RollbackPoints = GEQ

-- | Apply forward tip in RocksDB.
-- We compose csmt transactions for each operation with a updateRollbackPoint one
forwardTipApplyRocksDB
    :: (Ord key, Ord slot)
    => FromKV key value hash
    -- ^ FromKV instance to convert from key to Key and value to hash
    -> Hashing hash
    -- ^ Way to compose hashes
    -> slot
    -- ^ slot at which operations happen
    -> hash
    -- ^ hash of the block at which operations happen
    -> [Operation key value]
    -- ^ operations to apply
    -> Transaction IO ColumnFamily (Structure slot hash key value) BatchOp ()
forwardTipApplyRocksDB fromKV hashing slot hash ops = do
    invs <- forM ops $ \case
        Insert k v -> do
            inserting fromKV hashing KVCol CSMTCol k v
            pure $ Delete k
        Delete k -> do
            deleting fromKV hashing KVCol CSMTCol k
            mx <- query KVCol k
            case mx of
                Nothing ->
                    error
                        "forwardFinalityApplyRocksDB: cannot invert Delete operation, value not found"
                Just x -> pure $ Insert k x
    updateRollbackPoint slot hash invs

updateRollbackPoint
    :: Ord slot
    => slot
    -> hash
    -> [Operation key value]
    -> Transaction IO ColumnFamily (Structure slot hash key value) BatchOp ()
updateRollbackPoint slot hash ops =
    insert RollbackPoints slot
        $ RollBackPoint{rbpHash = hash, rbpInverseOperations = ops}
