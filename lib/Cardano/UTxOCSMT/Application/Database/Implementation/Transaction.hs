module Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction (..)
    , RunTransaction (..)
    , CSMTTransaction
    , CSMTContext (..)
    , insertCSMT
    , deleteCSMT
    , queryMerkleRoot
    , queryByAddress
    )
where

import CSMT (FromKV (..), Hashing, inserting)
import CSMT.Deletion (deleting)
import CSMT.Interface (Indirect (..), Key, root)
import CSMT.Proof.Completeness (collectValues)
import Control.Lens (review)
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Control.Monad.Reader
    ( MonadReader (..)
    , MonadTrans (..)
    , ReaderT (..)
    )
import Data.Maybe (catMaybes)
import Database.KV.Transaction
    ( Transaction
    , query
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

queryMerkleRoot
    :: Monad m
    => CSMTTransaction m cf op slot hash key value (Maybe hash)
queryMerkleRoot = do
    CSMTContext{hashing} <- lift . lift $ ask
    root hashing CSMTCol

-- | Query all UTxOs under a given address prefix.
-- Uses 'collectValues' to navigate the address-prefixed CSMT,
-- then reconstructs KV keys from leaf paths via the 'isoK' iso
-- in 'CSMTContext' and looks up values.
queryByAddress
    :: (Monad m, Ord key)
    => Key
    -- ^ Address prefix as CSMT Key
    -> CSMTTransaction m cf op slot hash key value [(key, value)]
queryByAddress addressKey = do
    CSMTContext{fromKV = FromKV{isoK}} <- lift . lift $ ask
    indirects <- collectValues CSMTCol addressKey
    fmap catMaybes $ traverse (lookupKV isoK) indirects
  where
    lookupKV isoK' Indirect{jump} = do
        let k = review isoK' jump
        mv <- query KVCol k
        pure $ fmap (k,) mv
