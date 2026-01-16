module Cardano.N2N.Client.Application.Database.Implementation.RunTransaction
    ( RunTransaction (..)
    )
where

import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Database.KV.Transaction
    ( Transaction
    )

-- | How to run a transaction
newtype RunTransaction cf op slot hash key value m = RunTransaction
    { transact
        :: forall a
         . Transaction m cf (Columns slot hash key value) op a
        -> m a
    }
