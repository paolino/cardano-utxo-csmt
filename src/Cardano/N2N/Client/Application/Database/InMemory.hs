module Cardano.N2N.Client.Application.Database.InMemory (InMemory (..), mkInMemoryDatabaseSimple)
where

import Cardano.N2N.Client.Application.Database.Interface
    ( Database (..)
    )
import Control.Monad.State.Strict (State)
import Data.Map.Strict (Map)
import Ouroboros.Network.Point (WithOrigin)

data InMemory slot key value = M
    { mUTxos :: Map key value
    , mSlots :: Map slot [key]
    , mTip :: WithOrigin slot
    }

mkInMemoryDatabaseSimple
    :: Database (State (InMemory slot key value)) slot key value
mkInMemoryDatabaseSimple =
    Database
        { forwardTip
        , rollbackTip
        , getValue
        , getTip
        , getFinality
        , forwardFinality
        , rollbackFinality
        }
  where
    forwardTip _slot _ops = undefined
    rollbackTip _slot = undefined
    getValue _key = undefined
    getTip = undefined
    getFinality = undefined
    forwardFinality _slot = undefined
    rollbackFinality = undefined
