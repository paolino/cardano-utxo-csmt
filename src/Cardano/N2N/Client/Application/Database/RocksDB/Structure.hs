module Cardano.N2N.Client.Application.Database.RocksDB.Structure
    ( Columns (..)
    , RollbackPoint (..)
    , RollbackPointKV
    , RollbackResult (..)
    , updateRocksDB
    , Armageddon (..)

      -- * Low level operations, exposed for testing
    , forwardFinalityApplyRocksDB
    , forwardTipApplyRocksDB
    , updateRollbackPoint
    , rollbackTipApplyRocksDB
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
    , KeyOf
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

-- | Represents a point in the blockchain. Hashes are needed to rule out time forks.
data Point slot hash = Point
    { pointSlot :: slot
    , pointHash :: hash
    }
    deriving (Show, Eq, Ord)

-- | Type alias for the RocksDB KV column storing rollback points
type RollbackPointKV slot hash key value =
    KV slot (RollbackPoint slot hash key value)

-- | Structure of the RocksDB database used by this application
data Columns slot hash key value x where
    KVCol :: Columns slot hash key value (KV key value)
        -- ^ Key-Value column for utxos
    CSMTCol :: Columns slot hash key value (KV Key (Indirect hash))
        -- ^ CSMT column for storing the CSMT of the UTxO set
    RollbackPoints
        :: Columns slot hash key value (RollbackPointKV slot hash key value)
        -- ^ Column for storing rollback points

-- | Parameters for performing an "armageddon" cleanup of the RocksDB database
data Armageddon slot hash key value = Armageddon
    { armageddonBatchSize :: Int
    -- ^ Number of entries to delete per batch
    , armageddonRunBatch
        :: forall a
         . Transaction IO ColumnFamily (Columns slot hash key value) BatchOp a
        -> IO a
    -- ^ Function to run a transaction, neeed to prevent memory exhaustion with
    -- an unbounded transaction size
    }

-- Clean up a RocksDB column batch
-- THIS IS NOT GOING TO RUN ATOMICALLY
cleanUpRocksDBBatch
    :: Ord (KeyOf x)
    => Columns slot hash key value x
    -> Armageddon slot hash key value
    -> IO ()
cleanUpRocksDBBatch column Armageddon{armageddonBatchSize, armageddonRunBatch} = do
    fix $ \transact -> do
        r <- armageddonRunBatch $ iterating column $ do
            me <- firstEntry
            ($ (me, 0)) $ fix $ \go -> \case
                (Nothing, _) -> pure False
                (_, n) | n >= armageddonBatchSize -> pure True
                (Just Entry{entryKey}, count) -> do
                    lift $ delete column entryKey
                    next <- nextEntry
                    go (next, count + 1)
        when r transact

-- Perform an "armageddon" cleanup of the RocksDB database
-- by deleting all entries in all columns in batches
-- THIS IS NOT GOING TO RUN ATOMICALLY
armageddonRocksDB
    :: (Ord key, Ord slot)
    => Armageddon slot hash key value
    -> IO ()
armageddonRocksDB armageddon = do
    cleanUpRocksDBBatch KVCol armageddon
    cleanUpRocksDBBatch CSMTCol armageddon
    cleanUpRocksDBBatch RollbackPoints armageddon

instance GEq (Columns slot hash key value) where
    geq KVCol KVCol = Just Refl
    geq CSMTCol CSMTCol = Just Refl
    geq RollbackPoints RollbackPoints = Just Refl
    geq _ _ = Nothing

instance GCompare (Columns slot hash key value) where
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
    -> Transaction IO ColumnFamily (Columns slot hash key value) BatchOp ()
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
    -> Transaction IO ColumnFamily (Columns slot hash key value) BatchOp ()
updateRollbackPoint Point{pointSlot, pointHash} rbpInverseOperations =
    insert RollbackPoints pointSlot
        $ RollbackPoint{rbpHash = pointHash, rbpInverseOperations}

sampleRollbackPoints
    :: Cursor
        (Transaction IO ColumnFamily (Columns slot hash key value) BatchOp)
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
    -> Transaction IO ColumnFamily (Columns slot hash key value) BatchOp ()
rollbackRollbackPoint fromKV hashing RollbackPoint{rbpInverseOperations} =
    forM_ rbpInverseOperations $ \case
        Insert k v -> inserting fromKV hashing KVCol CSMTCol k v
        Delete k -> deleting fromKV hashing KVCol CSMTCol k

-- | Result of a rollback attempt. Just a mirror, without continuation of 'Interface.State'
data RollbackResult slot hash
    = RollbackSucceeded
    | RollbackFailedButPossible [Point slot hash]
    | RollbackImpossible

-- | Create a RocksDB transaction that performs a rollback to the given slot
-- Returns whether the rollback was successful, failed but possible (with a list
-- of rollback points to intersect against), or impossible (in which case the
-- database should be truncated)
-- It DOES NOT encode the truncation as a transaction because that would potentially
-- be too big to fit in memory
-- Rollback is performed by seeking the exact rollback point, and then applying all
-- inverse operations down to that point excluded
-- If the exact rollback point is not found, we return a list of available rollback points
-- If the list is empty, rollback is impossible and the database should be truncated
rollbackTipApplyRocksDB
    :: (Ord slot, Eq hash, Ord key)
    => FromKV key value hash
    -- ^ FromKV instance to convert from key to Key and value to hash
    -> Hashing hash
    -- ^ Way to compose hashes
    -> WithOrigin (Point slot hash)
    -- ^ Slot to rollback to
    -> Transaction
        IO
        ColumnFamily
        (Columns slot hash key value)
        BatchOp
        (RollbackResult slot hash)
rollbackTipApplyRocksDB _fromKV _hashing Origin = pure RollbackImpossible
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

-- | Apply forward finality in RocksDB.
forwardFinalityApplyRocksDB
    :: Ord slot
    => Point slot hash
    -> Transaction IO ColumnFamily (Columns slot hash key value) BatchOp ()
forwardFinalityApplyRocksDB slot = do
    iterating RollbackPoints $ do
        me <- firstEntry
        ($ me) $ fix $ \go current ->
            case current of
                Nothing -> pure ()
                Just Entry{entryKey} -> when (entryKey <= pointSlot slot) $ do
                    lift $ delete RollbackPoints entryKey
                    nextEntry >>= go

-- | Update RocksDB database update state. This implementation does not take advantage
-- of continuations and so always propose itself as the next continuation
updateRocksDB
    :: (Ord key, Eq hash, Ord slot)
    => FromKV key value hash
    -- ^ FromKV instance to convert from key to Key and value to hash
    -> Hashing hash
    -- ^ Way to compose hashes
    -> Armageddon slot hash key value
    -- ^ Armageddon parameters, in case rollback is impossible
    -> Update
        (Transaction IO ColumnFamily (Columns slot hash key value) BatchOp)
        (Point slot hash)
        key
        value
updateRocksDB fromKV hashing armageddon = fix $ \u ->
    Update
        { forwardTipApply = \slot ops -> do
            forwardTipApplyRocksDB fromKV hashing slot ops
            pure u
        , rollbackTipApply = \slot -> do
            r <- rollbackTipApplyRocksDB fromKV hashing slot
            case r of
                RollbackSucceeded -> pure $ Syncing u
                RollbackFailedButPossible slots -> pure $ Intersecting slots u
                RollbackImpossible -> do
                    liftIO $ do
                        putStrLn
                            "rollbackTipApplyRocksDB: rollback impossible, truncating database"
                        armageddonRocksDB armageddon
                    pure $ Truncating u
        , forwardFinalityApply = \slot -> forwardFinalityApplyRocksDB slot >> pure u
        }
