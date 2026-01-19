module Cardano.N2N.Client.Application.Database.RocksDB
    ( RocksDBTransaction
    , RocksDBQuery
    , mkRunRocksDBTransaction
    , mkRunRocksDBCSMTTransaction
    , newRocksDBState
    )
where

import Cardano.N2N.Client.Application.Database.Implementation
    ( Columns
    , RunTransaction (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    )
import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Prisms
    , codecs
    )
import Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Update
    ( PartialHistory
    , newState
    )
import Cardano.N2N.Client.Application.Database.Interface
    ( Query
    , Update
    )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
    ( ReaderT (..)
    )
import Database.KV.RocksDB.Transaction (mkColumns, mkRocksDBDatabase)
import Database.KV.Transaction (Transaction, run)
import Database.RocksDB (BatchOp, ColumnFamily, DB)

type RocksDBTransaction m slot hash key value =
    Transaction m ColumnFamily (Columns slot hash key value) BatchOp

type RocksDBQuery m slot hash key value =
    Query m slot key value

runRocksDBTransaction
    :: (MonadFail m, MonadIO m)
    => Prisms slot hash key value
    -> DB
    -> Transaction
        m
        ColumnFamily
        (Columns slot hash key value)
        BatchOp
        b
    -> m b
runRocksDBTransaction prisms db =
    run
        $ mkRocksDBDatabase db
        $ mkColumns db
        $ codecs prisms

-- | Create a 'RunTransaction' for RocksDB
mkRunRocksDBTransaction
    :: (MonadIO m, MonadFail m)
    => DB
    -> Prisms slot hash key value
    -- ^ Prisms for serializing/deserializing keys and values
    -> RunTransaction ColumnFamily BatchOp slot hash key value m
mkRunRocksDBTransaction db prisms =
    RunTransaction $ \tx -> do
        runRocksDBTransaction prisms db tx

mkRunRocksDBCSMTTransaction
    :: (MonadIO m, MonadFail m)
    => DB
    -> Prisms slot hash key value
    -> CSMTContext hash key value
    -- ^ Prisms for serializing/deserializing keys and values
    -> RunCSMTTransaction
        ColumnFamily
        BatchOp
        slot
        hash
        key
        value
        m
mkRunRocksDBCSMTTransaction db prisms csmtContext =
    RunCSMTTransaction
        $ \tx -> flip runReaderT csmtContext $ do
            runRocksDBTransaction prisms db tx

newRocksDBState
    :: (MonadIO m, MonadFail m, Ord key, Ord slot)
    => DB
    -> Prisms slot hash key value
    -> CSMTContext hash key value
    -> PartialHistory
    -> (slot -> hash)
    -> ArmageddonParams hash
    -> m
        ( (Update m slot key value, [slot])
        , RunCSMTTransaction ColumnFamily BatchOp slot hash key value m
        )
newRocksDBState db prisms csmtContext partiality slotHash armageddonParams = do
    let runner = mkRunRocksDBCSMTTransaction db prisms csmtContext
    (,runner) <$> newState partiality slotHash armageddonParams runner
