module Cardano.N2N.Client.Application.Database.Interface
    ( Database (..)
    , Operation (..)
    , Update (..)
    , Truncated (..)

      -- * Implementation independent functions
    , inverseOp

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

-- | Indicates whether an update resulted in truncation of the database
data Truncated a = Truncated a | NotTruncated a

-- | Represents an update to the database
data Update m slot key value = Update
    { forwardTipApply
        :: slot
        -> [Operation key value]
        -> m (Update m slot key value)
    -- ^ Apply operations at the given slot, moving the tip forward
    , rollbackTipApply
        :: WithOrigin slot
        -> m (Truncated (Update m slot key value))
    -- ^ Rollback to the given slot, possibly truncating the database
    , forwardFinalityApply
        :: slot
        -> m (Update m slot key value)
    -- ^ Move the finality point forward
    }

data Database m slot key value = Database
    { getValue :: key -> m (Maybe value)
    , getTip :: m (WithOrigin slot)
    , getFinality :: m (WithOrigin slot)
    }

-- | Get the inverse of an operation, needs access to the database to retrieve
--   values for deletions
inverseOp
    :: Monad m
    => (key -> m (Maybe value))
    -> Operation key value
    -> m (Operation key value)
inverseOp value op = case op of
    Insert k _v -> pure $ Delete k
    Delete k -> do
        mv <- value k
        case mv of
            Just v -> pure $ Insert k v
            Nothing -> error "inverseOp: cannot invert Delete operation, value not found"

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
    -> Database m slot key value
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
