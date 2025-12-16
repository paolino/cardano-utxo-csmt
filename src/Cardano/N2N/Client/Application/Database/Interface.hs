module Cardano.N2N.Client.Application.Database.Interface
    ( Database (..)
    , Operation (..)

      -- * Database dump, for inspection/testing
    , Dump (..)
    , dumpDatabase
    )
where

import Ouroboros.Network.Point (WithOrigin)
import Prelude hiding (truncate)

data Operation key value
    = Insert key value
    | Delete key
    deriving (Show, Eq)

data Database m slot key value = Database
    { forwardTip :: slot -> [Operation key value] -> m ()
    , rollbackTip :: WithOrigin slot -> m Bool
    , getValue :: key -> m (Maybe value)
    , getTip :: m (WithOrigin slot)
    , getFinality :: m (WithOrigin slot)
    , forwardFinality :: slot -> m ()
    , rollbackFinality :: m ()
    }

data Dump slot key value = Dump
    { dumpTip :: WithOrigin slot
    , dumpFinality :: WithOrigin slot
    , dumpAssocs :: [(key, value)]
    }
    deriving (Show, Eq)

dumpDatabase
    :: Monad m
    => [key]
    -> Database m slot key value
    -> m (Dump slot key value)
dumpDatabase keys db = do
    tip <- getTip db
    immutable <- getFinality db
    contents <- traverse (\k -> fmap (k,) (getValue db k)) keys
    let presentContents = [(k, v) | (k, Just v) <- contents]
    pure
        $ Dump
            { dumpTip = tip
            , dumpFinality = immutable
            , dumpAssocs = presentContents
            }
