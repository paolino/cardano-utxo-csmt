module Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
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
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Control.Lens (review)
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

-- | CSMT context bundling the key/value codec and hashing operations.
data CSMTContext hash key value = CSMTContext
    { fromKV :: FromKV key value hash
    , hashing :: Hashing hash
    }

insertCSMT
    :: (Monad m, Ord key)
    => FromKV key value hash
    -> Hashing hash
    -> key
    -> value
    -> Transaction m cf (Columns slot hash key value) op ()
insertCSMT fkv h k v =
    inserting fkv h KVCol CSMTCol k v

deleteCSMT
    :: (Monad m, Ord key)
    => FromKV key value hash
    -> Hashing hash
    -> key
    -> Transaction m cf (Columns slot hash key value) op ()
deleteCSMT fkv h k =
    deleting fkv h KVCol CSMTCol k

queryMerkleRoot
    :: Monad m
    => Hashing hash
    -> Transaction m cf (Columns slot hash key value) op (Maybe hash)
queryMerkleRoot h =
    root h CSMTCol

{- | Query all UTxOs under a given address prefix.
Uses 'collectValues' to navigate the address-prefixed CSMT,
then reconstructs KV keys from leaf paths via the 'isoK' iso
and looks up values.
-}
queryByAddress
    :: (Monad m, Ord key)
    => FromKV key value hash
    -> Key
    -- ^ Address prefix as CSMT Key
    -> Transaction m cf (Columns slot hash key value) op [(key, value)]
queryByAddress FromKV{isoK} addressKey = do
    indirects <- collectValues CSMTCol addressKey
    catMaybes <$> traverse (lookupKV isoK) indirects
  where
    lookupKV isoK' Indirect{jump} = do
        let k = review isoK' jump
        mv <- query KVCol k
        pure $ fmap (k,) mv
