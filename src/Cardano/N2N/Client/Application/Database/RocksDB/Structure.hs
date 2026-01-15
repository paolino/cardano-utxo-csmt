module Cardano.N2N.Client.Application.Database.RocksDB.Structure
    ( Structure (..)
    , RollbackPoint (..)
    , RollbackPointKV
    , forwardTipApplyRocksDB
    , updateRollbackPoint
    , rollbackTipApplyRocksDB
    , RollbackResult (..)
    , forwardFinalityApplyRocksDB
    , updateRocksDB
    )
where

import CSMT (FromKV, Hashing, inserting)
import CSMT.Deletion (deleting)
import CSMT.Interface (Indirect, Key)
import Cardano.N2N.Client.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , Update (..)
    )
import Control.Lens ((:~:) (..))
import Control.Monad (forM, forM_, when)
import Control.Monad.Trans (MonadIO (..), lift)
import Data.Function (fix)
import Data.List.SampleFibonacci (sampleAtFibonacciIntervals)
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , lastEntry
    , nextEntry
    , prevEntry
    , seekKey
    )
import Database.KV.Transaction
    ( GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    , Transaction
    , delete
    , insert
    , iterating
    , query
    )
import Database.RocksDB (BatchOp, ColumnFamily)
import Ouroboros.Network.Point (WithOrigin (..))

-- | Represents a rollback point in the database
data RollbackPoint slot hash key value = RollbackPoint
    { rbpHash :: hash
    , rbpInverseOperations :: [Operation key value]
    }

data Point slot hash = Point
    { pointSlot :: slot
    , pointHash :: hash
    }
    deriving (Show, Eq, Ord)

-- | Type alias for the RocksDB KV column storing rollback points
type RollbackPointKV slot hash key value =
    KV slot (RollbackPoint slot hash key value)

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
    -> Point slot hash
    -- ^ slot at which operations happen
    -> [Operation key value]
    -- ^ operations to apply
    -> Transaction IO ColumnFamily (Structure slot hash key value) BatchOp ()
forwardTipApplyRocksDB fromKV hashing slot ops = do
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
    updateRollbackPoint slot invs

updateRollbackPoint
    :: Ord slot
    => Point slot hash
    -> [Operation key value]
    -> Transaction IO ColumnFamily (Structure slot hash key value) BatchOp ()
updateRollbackPoint Point{pointSlot, pointHash} rbpInverseOperations =
    insert RollbackPoints pointSlot
        $ RollbackPoint{rbpHash = pointHash, rbpInverseOperations}

sampleRollbackPoints
    :: Cursor
        (Transaction IO ColumnFamily (Structure slot hash key value) BatchOp)
        (RollbackPointKV slot hash key value)
        [Point slot hash]
sampleRollbackPoints = do
    fmap mkPoint <$> sampleAtFibonacciIntervals prevEntry
  where
    mkPoint Entry{entryKey, entryValue = RollbackPoint{rbpHash}} =
        Point{pointSlot = entryKey, pointHash = rbpHash}

rollbackRollbackPoint
    :: Ord key
    => FromKV key value hash
    -> Hashing hash
    -> RollbackPoint slot hash key value
    -> Transaction IO ColumnFamily (Structure slot hash key value) BatchOp ()
rollbackRollbackPoint fromKV hashing RollbackPoint{rbpInverseOperations} =
    forM_ rbpInverseOperations $ \case
        Insert k v -> inserting fromKV hashing KVCol CSMTCol k v
        Delete k -> deleting fromKV hashing KVCol CSMTCol k

data RollbackResult slot hash
    = RollbackSucceeded
    | RollbackFailedButPossible [Point slot hash]
    | RollbackImpossible

rollbackTipApplyRocksDB
    :: (Ord slot, Eq hash, Ord key)
    => FromKV key value hash
    -> Hashing hash
    -> WithOrigin (Point slot hash)
    -> Transaction
        IO
        ColumnFamily
        (Structure slot hash key value)
        BatchOp
        (RollbackResult slot hash)
rollbackTipApplyRocksDB _fromKV _hashing Origin = error "not implemented yet"
rollbackTipApplyRocksDB fromKV hashing (At slot) = do
    iterating RollbackPoints $ do
        me <- seekKey $ pointSlot slot
        case me of
            Nothing -> pure RollbackImpossible
            Just (Entry foundSlot RollbackPoint{rbpHash = foundHash}) -> do
                if Point{pointSlot = foundSlot, pointHash = foundHash} == slot
                    then do
                        ml <- lastEntry
                        ($ ml) $ fix $ \go current -> case current of
                            Nothing -> liftIO $ fail "rollbackTipApply: inconsistent rollback points"
                            Just Entry{entryKey, entryValue} ->
                                when (entryKey > pointSlot slot) $ do
                                    lift $ do
                                        rollbackRollbackPoint fromKV hashing entryValue
                                        delete RollbackPoints entryKey
                                    prevEntry >>= go
                        pure RollbackSucceeded
                    else RollbackFailedButPossible <$> sampleRollbackPoints

forwardFinalityApplyRocksDB
    :: Ord slot
    => Point slot hash
    -> Transaction IO ColumnFamily (Structure slot hash key value) BatchOp ()
forwardFinalityApplyRocksDB slot = do
    iterating RollbackPoints $ do
        me <- firstEntry
        ($ me) $ fix $ \go current ->
            case current of
                Nothing -> pure ()
                Just Entry{entryKey} -> when (entryKey <= pointSlot slot) $ do
                    lift $ delete RollbackPoints entryKey
                    nextEntry >>= go

updateRocksDB
    :: (Ord key, Eq hash, Ord slot)
    => FromKV key value hash
    -> Hashing hash
    -> Update
        (Transaction IO ColumnFamily (Structure slot hash key value) BatchOp)
        (Point slot hash)
        key
        value
updateRocksDB fromKV hashing = fix $ \u ->
    Update
        { forwardTipApply = \slot ops -> do
            forwardTipApplyRocksDB fromKV hashing slot ops
            pure u
        , rollbackTipApply = \slot -> do
            r <- rollbackTipApplyRocksDB fromKV hashing slot
            case r of
                RollbackSucceeded -> pure $ Syncing u
                RollbackFailedButPossible slots -> pure $ Intersecting slots u
                RollbackImpossible -> pure $ Truncating u
        , forwardFinalityApply = \slot -> forwardFinalityApplyRocksDB slot >> pure u
        }
