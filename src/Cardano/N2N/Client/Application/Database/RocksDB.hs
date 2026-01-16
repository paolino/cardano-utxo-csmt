module Cardano.N2N.Client.Application.Database.RocksDB
    ( RocksDBTransaction
    , RocksDBQuery
    , mkRunRocksDBTransaction
    , mkRunRocksDBCSMTTransaction
    )
where

import Cardano.N2N.Client.Application.Database.Implementation
    ( Columns
    , Point
    , RunTransaction (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Prisms
    , codecs
    )
import Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    )
import Cardano.N2N.Client.Application.Database.Interface
    ( Query
    )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
    ( MonadReader (..)
    , MonadTrans (..)
    , ReaderT (..)
    )
import Database.KV.RocksDB.Transaction (mkColumns, mkRocksDBDatabase)
import Database.KV.Transaction (Transaction, run)
import Database.RocksDB (BatchOp, ColumnFamily, DB)

type RocksDBTransaction m slot hash key value =
    Transaction m ColumnFamily (Columns slot hash key value) BatchOp

type RocksDBQuery m slot hash key value =
    Query m (Point slot hash) key value

-- | Create a 'RunTransaction' for RocksDB
mkRunRocksDBTransaction
    :: (MonadIO m, MonadFail m)
    => Prisms slot hash key value
    -- ^ Prisms for serializing/deserializing keys and values
    -> RunTransaction ColumnFamily BatchOp slot hash key value (ReaderT DB m)
mkRunRocksDBTransaction prisms =
    RunTransaction $ \tx -> do
        db <- ask
        run
            ( mkRocksDBDatabase db
                $ mkColumns db
                $ codecs prisms
            )
            tx

mkRunRocksDBCSMTTransaction
    :: (MonadIO m, MonadFail m)
    => Prisms slot hash key value
    -> CSMTContext hash key value
    -- ^ Prisms for serializing/deserializing keys and values
    -> RunCSMTTransaction
        ColumnFamily
        BatchOp
        slot
        hash
        key
        value
        (ReaderT DB m)
mkRunRocksDBCSMTTransaction prisms csmtContext =
    RunCSMTTransaction
        $ \tx -> flip runReaderT csmtContext $ do
            db <- lift ask
            run
                ( mkRocksDBDatabase db
                    $ mkColumns db
                    $ codecs prisms
                )
                tx
