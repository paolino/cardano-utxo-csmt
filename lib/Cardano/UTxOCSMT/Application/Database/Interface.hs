module Cardano.UTxOCSMT.Application.Database.Interface
    ( -- * Query interface
      Query (..)
    , hoistQuery

      -- * Operation
    , Operation (..)
    , inverseOp

      -- * Update interface
    , Update (..)
    , State (..)

      -- * Database dump, for inspection/testing
    , Dump (..)
    , dumpDatabase
    , emptyDump
    )
where

import Data.List (sortOn)
import Ouroboros.Network.Point (WithOrigin (..))
import Prelude hiding (truncate)

-- | Represents an operation on the database
data Operation key value
    = Insert key value
    | Delete key
    deriving (Show, Eq)

-- | State of the database update process
data State m slot key value
    = -- | Database is syncing, accepts forward and rollback operations
      Syncing (Update m slot key value)
    | -- | Database is intersecting, has a list of slots to intersect against
      -- Contrary to what one believes there is nothing to do with the choosen slot
      -- Only rollbacks can really move the tip
      Intersecting [slot] (Update m slot key value)
    | -- | Database is truncating, no possible rollbacks, the protocol should reset to Origin
      Truncating (Update m slot key value)

-- | Represents an update to the database. We offer a continuation-based API so that
-- the database implementation can thread an internal state without messing up with the
-- monad stack.
-- Valid consumers should always pick the continuation
data Update m slot key value = Update
    { forwardTipApply
        :: slot
        -> [Operation key value]
        -> m (Update m slot key value)
    -- ^ Apply operations at the given slot, moving the tip forward
    , rollbackTipApply
        :: WithOrigin slot
        -> m (State m slot key value)
    -- ^ Rollback to the given slot, possibly truncating the database
    , forwardFinalityApply
        :: slot
        -> m (Update m slot key value)
    -- ^ Move the finality point forward. It's a bit of a pain to have to compute the slot
    -- as we pratically need to keep a window of the changes.
    }

data Query m slot key value = Query
    { getValue :: key -> m (Maybe value)
    , getTip :: m (WithOrigin slot)
    , getFinality :: m (WithOrigin slot)
    }

-- | Let a transaction runner apply to all queries
hoistQuery
    :: (forall a. m a -> n a)
    -> Query m slot key value
    -> Query n slot key value
hoistQuery nat Query{getValue, getTip, getFinality} =
    Query
        { getValue = nat . getValue
        , getTip = nat getTip
        , getFinality = nat getFinality
        }

-- | Get the inverse of an operation, needs access to the database to retrieve
--   values for deletions
inverseOp
    :: Monad m
    => (key -> m (Maybe value))
    -> Operation key value
    -> m (Maybe (Operation key value))
inverseOp value op = case op of
    Insert k _v -> pure $ Just (Delete k)
    Delete k -> do
        mv <- value k
        case mv of
            Just v -> pure $ Just (Insert k v)
            Nothing -> pure Nothing

-- | A dump of the database contents for inspection/testing
data Dump slot key value = Dump
    { dumpTip :: WithOrigin slot
    , dumpFinality :: WithOrigin slot
    , dumpAssocs :: [(key, value)]
    }
    deriving (Show, Eq)

-- | An empty database dump
emptyDump :: Dump slot key value
emptyDump =
    Dump
        { dumpTip = Origin
        , dumpFinality = Origin
        , dumpAssocs = []
        }

-- | Dump the contents of the database for the given keys. It's up to the caller
--   to provide the keys of interest.
dumpDatabase
    :: (Monad m, Ord key)
    => [key]
    -> Query m slot key value
    -> m (Dump slot key value)
dumpDatabase keys db = do
    tip <- getTip db
    immutable <- getFinality db
    contents <- traverse (\k -> fmap (k,) (getValue db k)) keys
    let presentContents = sortOn fst $ [(k, v) | (k, Just v) <- contents]
    pure
        $ Dump
            { dumpTip = tip
            , dumpFinality = immutable
            , dumpAssocs = presentContents
            }
