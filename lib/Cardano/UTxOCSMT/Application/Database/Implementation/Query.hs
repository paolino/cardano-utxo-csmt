module Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( mkQuery
    , mkTransactionedQuery
    , getAllMerkleRoots
    , putBaseCheckpoint
    , getBaseCheckpoint
    , isBootstrapInProgress
    , setBootstrapInProgress
    , clearBootstrapInProgress
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , ConfigKey (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Query (..)
    , hoistQuery
    )
import Control.Monad.Trans (lift)
import Data.Function (fix)
import Data.Maybe (isJust)
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , lastEntry
    , prevEntry
    )
import Database.KV.Transaction
    ( Transaction
    , delete
    , insert
    , iterating
    , query
    )
import Ouroboros.Network.Point (WithOrigin (..))

-- | Create an query interface
mkQuery
    :: (Ord key, MonadFail m)
    => Query
        (Transaction m cf (Columns slot hash key value) op)
        slot
        key
        value
mkQuery =
    Query
        { getValue = query KVCol
        , getTip =
            iterating RollbackPoints $ do
                ml <- lastEntry
                case ml of
                    Nothing -> lift . lift $ fail "No tip in rollback points"
                    Just e -> pure $ entryKey e
        , getFinality =
            iterating RollbackPoints $ do
                mf <- firstEntry
                case mf of
                    Nothing -> lift . lift $ fail "No finality point in rollback points"
                    Just e -> pure $ entryKey e
        }

{- | Create a 'Query' interface for RocksDB where all queries are run in separate transactions
Useful for property testing
-}
mkTransactionedQuery
    :: (Ord key, MonadFail m)
    => RunTransaction cf op slot hash key value m
    -> Query m slot key value
mkTransactionedQuery (RunTransaction runTx) = hoistQuery runTx mkQuery

{- | Get all merkle roots by iterating in reverse over the RollbackPoints table
Returns a list of (slot, blockHash, merkleRoot) tuples in reverse order (newest first)
-}
getAllMerkleRoots
    :: Monad m
    => Transaction
        m
        cf
        (Columns slot hash key value)
        op
        [(WithOrigin slot, hash, Maybe hash)]
getAllMerkleRoots =
    iterating RollbackPoints $ do
        ml <- lastEntry
        ($ ml) $ fix $ \go current -> case current of
            Nothing -> pure []
            Just
                Entry{entryKey, entryValue = RollbackPoint{rbpHash, rpbMerkleRoot}} -> do
                    rest <- prevEntry >>= go
                    pure $ (entryKey, rbpHash, rpbMerkleRoot) : rest

getBaseCheckpoint
    :: Transaction m cf (Columns slot hash key value) op (Maybe slot)
getBaseCheckpoint = query ConfigCol BaseCheckpointKey

putBaseCheckpoint
    :: slot
    -> Transaction m cf (Columns slot hash key value) op ()
putBaseCheckpoint = insert ConfigCol BaseCheckpointKey

-- | Check if bootstrap is in progress (incomplete)
isBootstrapInProgress
    :: Monad m
    => Transaction m cf (Columns slot hash key value) op Bool
isBootstrapInProgress = isJust <$> query ConfigCol BootstrapInProgressKey

-- | Mark bootstrap as in progress (stores target slot for reference)
setBootstrapInProgress
    :: slot
    -> Transaction m cf (Columns slot hash key value) op ()
setBootstrapInProgress = insert ConfigCol BootstrapInProgressKey

-- | Clear bootstrap in progress marker
clearBootstrapInProgress
    :: Transaction m cf (Columns slot hash key value) op ()
clearBootstrapInProgress = delete ConfigCol BootstrapInProgressKey
