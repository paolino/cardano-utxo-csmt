module Cardano.N2N.Client.Application.Database.Interface
    ( Database (..)
    , Operation (..)
    , Update (..)
    , UpdateBox (..)
    , ProgressK (..)
    , UpdateResult (..)
    , Event (..)

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

data Operation key value
    = Insert key value
    | Delete key
    deriving (Show, Eq)

data Database m slot key value = Database
    { getValue :: key -> m (Maybe value)
    , getTip :: m (WithOrigin slot)
    , getFinality :: m (WithOrigin slot)
    }

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

data Dump slot key value = Dump
    { dumpTip :: WithOrigin slot
    , dumpFinality :: WithOrigin slot
    , dumpAssocs :: [(key, value)]
    }
    deriving (Show, Eq)

emptyDump :: Dump slot key value
emptyDump =
    Dump
        { dumpTip = Origin
        , dumpFinality = Origin
        , dumpAssocs = []
        }

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

data ProgressK = ProgressT | RewindT

data Event slot key value where
    ForwardTip :: slot -> [Operation key value] -> Event slot key value
    ForwardFinality :: slot -> Event slot key value
    RollbackTip :: WithOrigin slot -> Event slot key value

data UpdateResult (p :: ProgressK) m slot key value where
    Progress
        :: Update 'ProgressT m slot key value
        -> UpdateResult 'ProgressT m slot key value
    Rewind
        :: Update 'RewindT m slot key value
        -> UpdateResult 'RewindT m slot key value

data Update (p :: ProgressK) m slot key value where
    TakeEvent
        :: (Event slot key value -> m (UpdateBox m slot key value))
        -> Update 'ProgressT m slot key value
    Truncate
        :: m (UpdateResult 'ProgressT m slot key value)
        -> Update 'RewindT m slot key value

data UpdateBox m slot key value where
    UpdateBox
        :: UpdateResult p m slot key value -> UpdateBox m slot key value
