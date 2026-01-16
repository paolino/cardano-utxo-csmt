module Cardano.N2N.Client.Application.Database.Implementation
    ( Columns (..)
    , RollbackPoint (..)
    , RollbackPointKV
    , RollbackResult (..)
    , Point (..)
    , mkUpdate
    , RunTransaction (..)

      -- * Low level operations, exposed for testing
    , forwardFinality
    , forwardTip
    , updateRollbackPoint
    , rollbackTip
    , mkQuery
    , rollbackPointPrism
    )
where

import CSMT (FromKV, Hashing, inserting)
import CSMT.Deletion (deleting)
import Cardano.N2N.Client.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    , armageddon
    )
import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Point
    ( Point (..)
    , mkPoint
    )
import Cardano.N2N.Client.Application.Database.Implementation.Query
    ( mkQuery
    )
import Cardano.N2N.Client.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    , RollbackPointKV
    , rollbackPointPrism
    )
import Cardano.N2N.Client.Application.Database.Implementation.RunTransaction
    ( RunTransaction (..)
    )
import Cardano.N2N.Client.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , Update (..)
    )
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
    ( Transaction
    , delete
    , insert
    , iterating
    , query
    )
import Ouroboros.Network.Point (WithOrigin (..))

-- | Apply forward tip .
-- We compose csmt transactions for each operation with a updateRollbackPoint one
forwardTip
    :: (Ord key, Ord slot, Monad m)
    => FromKV key value hash
    -- ^ FromKV instance to convert from key to Key and value to hash
    -> Hashing hash
    -- ^ Way to compose hashes
    -> Point slot hash
    -- ^ slot at which operations happen
    -> [Operation key value]
    -- ^ operations to apply
    -> Transaction m cf (Columns slot hash key value) op ()
forwardTip fromKV hashing slot ops = do
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
                        "forwardFinalityApply: cannot invert Delete operation, value not found"
                Just x -> pure $ Insert k x
    updateRollbackPoint slot invs

updateRollbackPoint
    :: (Ord slot)
    => Point slot hash
    -> [Operation key value]
    -> Transaction m cf (Columns slot hash key value) op ()
updateRollbackPoint Point{pointSlot, pointHash} rbpInverseOperations =
    insert RollbackPoints pointSlot
        $ RollbackPoint{rbpHash = pointHash, rbpInverseOperations}

sampleRollbackPoints
    :: Monad m
    => Cursor
        (Transaction m cf (Columns slot hash key value) op)
        (RollbackPointKV slot hash key value)
        [Point slot hash]
sampleRollbackPoints = do
    fmap mkPoint <$> sampleAtFibonacciIntervals prevEntry

rollbackRollbackPoint
    :: (Ord key, Monad m)
    => FromKV key value hash
    -> Hashing hash
    -> RollbackPoint slot hash key value
    -> Transaction m cf (Columns slot hash key value) op ()
rollbackRollbackPoint fromKV hashing RollbackPoint{rbpInverseOperations} =
    forM_ rbpInverseOperations $ \case
        Insert k v -> inserting fromKV hashing KVCol CSMTCol k v
        Delete k -> deleting fromKV hashing KVCol CSMTCol k

-- | Result of a rollback attempt. Just a mirror, without continuation of 'Interface.State'
data RollbackResult slot hash
    = RollbackSucceeded
    | RollbackFailedButPossible [Point slot hash]
    | RollbackImpossible

-- | Create a transaction that performs a rollback to the given slot
-- Returns whether the rollback was successful, failed but possible (with a list
-- of rollback points to intersect against), or impossible (in which case the
-- database should be truncated)
-- It DOES NOT encode the truncation as a transaction because that would potentially
-- be too big to fit in memory
-- Rollback is performed by seeking the exact rollback point, and then applying all
-- inverse operations down to that point excluded
-- If the exact rollback point is not found, we return a list of available rollback points
-- If the list is empty, rollback is impossible and the database should be truncated
rollbackTip
    :: (Ord slot, Eq hash, Ord key, MonadFail m)
    => FromKV key value hash
    -- ^ FromKV instance to convert from key to Key and value to hash
    -> Hashing hash
    -- ^ Way to compose hashes
    -> Point slot hash
    -- ^ Slot to rollback to
    -> Transaction
        m
        cf
        (Columns slot hash key value)
        op
        (RollbackResult slot hash)
rollbackTip fromKV hashing slot = do
    iterating RollbackPoints $ do
        me <- seekKey $ pointSlot slot
        case me of
            Nothing -> pure RollbackImpossible
            Just (Entry foundSlot RollbackPoint{rbpHash = foundHash}) -> do
                if Point{pointSlot = foundSlot, pointHash = foundHash} == slot
                    then do
                        ml <- lastEntry
                        ($ ml) $ fix $ \go current -> case current of
                            Nothing ->
                                lift . lift $ fail "rollbackTipApply: inconsistent rollback points"
                            Just Entry{entryKey, entryValue} ->
                                when (entryKey > pointSlot slot) $ do
                                    lift $ do
                                        rollbackRollbackPoint fromKV hashing entryValue
                                        delete RollbackPoints entryKey
                                    prevEntry >>= go
                        pure RollbackSucceeded
                    else RollbackFailedButPossible <$> sampleRollbackPoints

-- | Apply forward finality .
forwardFinality
    :: (Ord slot, Monad m)
    => Point slot hash
    -> Transaction m cf (Columns slot hash key value) op ()
forwardFinality slot = do
    iterating RollbackPoints $ do
        me <- firstEntry
        ($ me) $ fix $ \go current ->
            case current of
                Nothing -> pure ()
                Just Entry{entryKey} -> when (entryKey <= pointSlot slot) $ do
                    lift $ delete RollbackPoints entryKey
                    nextEntry >>= go

-- | Create an database update state object. This implementation does not take advantage
-- of continuations and so always propose itself as the next continuation
mkUpdate
    :: (Ord key, Eq hash, Ord slot, MonadFail m, MonadIO m)
    => FromKV key value hash
    -- ^ FromKV instance to convert from key to Key and value to hash
    -> Hashing hash
    -- ^ Way to compose hashes
    -> RunTransaction cf op slot hash key value m
    -- ^ Function to run a transaction
    -> ArmageddonParams
    -- ^ Armageddon parameters, in case rollback is impossible
    -> Update
        m
        (Point slot hash)
        key
        value
mkUpdate fromKV hashing runTransaction@RunTransaction{transact} armageddonParams =
    fix $ \cont ->
        Update
            { forwardTipApply = \slot ops -> transact $ do
                forwardTip fromKV hashing slot ops
                pure cont
            , rollbackTipApply = \case
                At slot -> do
                    r <- transact $ rollbackTip fromKV hashing slot
                    case r of
                        RollbackSucceeded -> pure $ Syncing cont
                        RollbackFailedButPossible slots -> pure $ Intersecting slots cont
                        RollbackImpossible -> pure $ Truncating cont
                Origin -> do
                    liftIO $ do
                        putStrLn
                            "rollbackTipApply: rollback to Origin, truncating database"
                    armageddon runTransaction armageddonParams
                    pure $ Syncing cont
            , forwardFinalityApply = \slot -> transact $ forwardFinality slot >> pure cont
            }
