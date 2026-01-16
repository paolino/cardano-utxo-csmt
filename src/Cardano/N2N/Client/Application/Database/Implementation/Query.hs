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
import Cardano.N2N.Client.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    )
import Cardano.N2N.Client.Application.Database.Interface
    ( Query (..)
    , hoistQuery
    )
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , lastEntry
    )
import Database.KV.Transaction
    ( KV
    , Transaction
    , iterating
    , query
    )
import Ouroboros.Network.Point (WithOrigin (..))

-- | Create an query interface
mkQuery
    :: (Ord key, Monad m)
    => Query
        (Transaction m cf (Columns slot hash key value) op)
        (Point slot hash)
        key
        value
mkQuery =
    Query
        { getValue = query KVCol
        , getTip =
            iterating RollbackPoints $ rollbackPointDefaultToOrigin <$> lastEntry
        , getFinality =
            iterating RollbackPoints $ rollbackPointDefaultToOrigin <$> firstEntry
        }

rollbackPointDefaultToOrigin
    :: Maybe (Entry (KV slot (RollbackPoint slot hash key value)))
    -> WithOrigin (Point slot hash)
rollbackPointDefaultToOrigin Nothing = Origin
rollbackPointDefaultToOrigin (Just e) = At $ mkPoint e

-- | Create a 'Query' interface for RocksDB where all queries are run in separate transactions
-- Useful for property testing
mkTransactionedQuery
    :: (Ord key, Monad m)
    => RunTransaction cf op slot hash key value m
    -> Query m (Point slot hash) key value
mkTransactionedQuery (RunTransaction runTx) = hoistQuery runTx mkQuery
