module Cardano.N2N.Client.Application.Database.Implementation.Update
    ( mkUpdate

      -- * Low level operations, exposed for testing
    , forwardFinality
    , forwardTip
    , updateRollbackPoint
    , rollbackTip
    )
where

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
import Cardano.N2N.Client.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    , RollbackPointKV
    )
import Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( CSMTTransaction
    , RunCSMTTransaction (..)
    , deleteCSMT
    , insertCSMT
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
    ( delete
    , insert
    , iterating
    , query
    )
import Ouroboros.Network.Point (WithOrigin (..))

-- | Apply forward tip .
-- We compose csmt transactions for each operation with an updateRollbackPoint one
forwardTip
    :: (Ord key, Ord slot, Monad m)
    => Point slot hash
    -- ^ slot at which operations happen
    -> [Operation key value]
    -- ^ operations to apply
    -> CSMTTransaction m cf op slot hash key value ()
forwardTip slot ops = do
    invs <- forM ops $ \case
        Insert k v -> do
            insertCSMT k v
            pure [Delete k]
        Delete k -> do
            deleteCSMT k
            mx <- query KVCol k
            case mx of
                Nothing -> pure []
                -- error
                -- "forwardFinalityApply: cannot invert Delete operation, value not found"
                Just x -> pure [Insert k x]
    updateRollbackPoint slot $ concat invs

updateRollbackPoint
    :: (Ord slot)
    => Point slot hash
    -> [Operation key value]
    -> CSMTTransaction m cf op slot hash key value ()
updateRollbackPoint Point{pointSlot, pointHash} rbpInverseOperations =
    insert RollbackPoints pointSlot
        $ RollbackPoint{rbpHash = pointHash, rbpInverseOperations}

sampleRollbackPoints
    :: Monad m
    => Cursor
        (CSMTTransaction m cf op slot hash key value)
        (RollbackPointKV slot hash key value)
        [Point slot hash]
sampleRollbackPoints = do
    fmap mkPoint <$> sampleAtFibonacciIntervals prevEntry

rollbackRollbackPoint
    :: (Ord key, Monad m)
    => RollbackPoint slot hash key value
    -> CSMTTransaction m cf op slot hash key value ()
rollbackRollbackPoint RollbackPoint{rbpInverseOperations} =
    forM_ rbpInverseOperations $ \case
        Insert k v -> insertCSMT k v
        Delete k -> deleteCSMT k

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
    => Point slot hash
    -- ^ Slot to rollback to
    -> CSMTTransaction m cf op slot hash key value (RollbackResult slot hash)
rollbackTip slot = do
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
                                        rollbackRollbackPoint entryValue
                                        delete RollbackPoints entryKey
                                    prevEntry >>= go
                        pure RollbackSucceeded
                    else RollbackFailedButPossible <$> sampleRollbackPoints

-- | Apply forward finality .
forwardFinality
    :: (Ord slot, Monad m)
    => Point slot hash
    -> CSMTTransaction m cf op slot hash key value ()
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
    => ArmageddonParams
    -- ^ Armageddon parameters, in case rollback is impossible
    -> RunCSMTTransaction cf op slot hash key value m
    -- ^ Function to run a transaction
    -> Update m (Point slot hash) key value
mkUpdate armageddonParams runTransaction@RunCSMTTransaction{txRunTransaction} =
    fix $ \cont ->
        Update
            { forwardTipApply = \slot ops -> txRunTransaction $ do
                forwardTip slot ops
                pure cont
            , rollbackTipApply = \case
                At slot -> do
                    r <- txRunTransaction $ rollbackTip slot
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
            , forwardFinalityApply = \slot -> txRunTransaction $ forwardFinality slot >> pure cont
            }
