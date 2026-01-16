module Cardano.N2N.Client.Application.Database.Implementation.Query
    ( mkQuery
    , mkTransactionedQuery
    )
where

import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Point
    ( Point (..)
    , mkPoint
    )
import Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    )
import Cardano.N2N.Client.Application.Database.Interface
    ( Query (..)
    , hoistQuery
    )
import Control.Monad.Trans (lift)
import Database.KV.Cursor
    ( firstEntry
    , lastEntry
    )
import Database.KV.Transaction
    ( Transaction
    , iterating
    , query
    )

-- | Create an query interface
mkQuery
    :: (Ord key, MonadFail m)
    => Query
        (Transaction m cf (Columns slot hash key value) op)
        (Point slot hash)
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
                    Just e -> pure $ mkPoint e
        , getFinality =
            iterating RollbackPoints $ do
                mf <- firstEntry
                case mf of
                    Nothing -> lift . lift $ fail "No finality point in rollback points"
                    Just e -> pure $ mkPoint e
        }

-- | Create a 'Query' interface for RocksDB where all queries are run in separate transactions
-- Useful for property testing
mkTransactionedQuery
    :: (Ord key, MonadFail m)
    => RunTransaction cf op slot hash key value m
    -> Query m (Point slot hash) key value
mkTransactionedQuery (RunTransaction runTx) = hoistQuery runTx mkQuery
