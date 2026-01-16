module Cardano.N2N.Client.Application.Database.Implementation.Point
    ( Point (..)
    , mkPoint
    )
where

import Cardano.N2N.Client.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    , RollbackPointKV
    )
import Control.Lens ((<&>))
import Database.KV.Cursor
    ( Entry (..)
    )
import Ouroboros.Network.Point (WithOrigin)

-- | Represents a point in the blockchain. Hashes are needed to rule out time forks.
data Point slot hash = Point
    { pointSlot :: slot
    , pointHash :: hash
    }
    deriving (Show)

-- The following broken instances are BAD. TODO: percolate the hash type up to the interface.
instance Eq slot => Eq (Point slot hash) where
    Point{pointSlot = slot1} == Point{pointSlot = slot2} =
        slot1 == slot2

instance Ord slot => Ord (Point slot hash) where
    compare Point{pointSlot = slot1} Point{pointSlot = slot2} =
        compare slot1 slot2

-- | Create a 'Point' from a database entry of the 'RollbackPoint' column
mkPoint
    :: Entry (RollbackPointKV slot hash key value)
    -> WithOrigin (Point slot hash)
mkPoint Entry{entryKey, entryValue = RollbackPoint{rbpHash}} =
    entryKey <&> \rentryKey -> Point{pointSlot = rentryKey, pointHash = rbpHash}
