module Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction (..)
    , RunTransaction (..)
    , CSMTTransaction
    , CSMTContext (..)
    , insertCSMT
    , deleteCSMT
    )
where

import CSMT (FromKV, Hashing, inserting)
import CSMT.Deletion (deleting)
import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Control.Monad.Reader
    ( MonadReader (..)
    , MonadTrans (..)
    , ReaderT (..)
    )
import Database.KV.Transaction
    ( Transaction
    )

newtype RunTransaction cf op slot hash key value m = RunTransaction
    { transact
        :: forall a
         . Transaction m cf (Columns slot hash key value) op a
        -> m a
    }

data CSMTContext hash key value = CSMTContext
    { fromKV :: FromKV key value hash
    , hashing :: Hashing hash
    }

type CSMTTransaction m cf op slot hash key value =
    Transaction
        (ReaderT (CSMTContext hash key value) m)
        cf
        (Columns slot hash key value)
        op

newtype RunCSMTTransaction cf op slot hash key value m = RunCSMTTransaction
    { txRunTransaction
        :: forall a
         . CSMTTransaction m cf op slot hash key value a
        -> m a
    }

insertCSMT
    :: (Monad m, Ord key)
    => key -> value -> CSMTTransaction m cf op slot hash key value ()
insertCSMT k v = do
    CSMTContext{fromKV, hashing} <- lift . lift $ ask
    inserting fromKV hashing KVCol CSMTCol k v

deleteCSMT
    :: (Monad m, Ord key)
    => key -> CSMTTransaction m cf op slot hash key value ()
deleteCSMT k = do
    CSMTContext{fromKV, hashing} <- lift . lift $ ask
    deleting fromKV hashing KVCol CSMTCol k
