module Cardano.N2N.Client.Application.Database.Implementation.Point
    ( Point (..)
    , mkPoint
    )
where

import Cardano.N2N.Client.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    )
import Database.KV.Cursor
    ( Entry (..)
    )
import Database.KV.Transaction
    ( KV
    )

-- | Represents a point in the blockchain. Hashes are needed to rule out time forks.
data Point slot hash = Point
    { pointSlot :: slot
    , pointHash :: hash
    }
    deriving (Show, Eq, Ord)

-- | Create a 'Point' from a database entry of the 'RollbackPoint' column
mkPoint
    :: Entry (KV slot (RollbackPoint slot hash key value)) -> Point slot hash
mkPoint Entry{entryKey, entryValue = RollbackPoint{rbpHash}} =
    Point{pointSlot = entryKey, pointHash = rbpHash}
