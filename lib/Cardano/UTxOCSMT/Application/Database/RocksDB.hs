{- |
Module      : Cardano.UTxOCSMT.Application.Database.RocksDB
Description : RocksDB-backed database implementation

This module provides the RocksDB backend for the UTxO CSMT database.
It handles:

* Transaction management with atomic batch operations
* Column family setup for different data types (UTxOs, CSMT, rollback points)
* Integration with the abstract 'Update' and 'Query' interfaces
-}
module Cardano.UTxOCSMT.Application.Database.RocksDB
    ( RocksDBTransaction
    , RocksDBQuery
    , newRunRocksDBTransaction
    , newRunRocksDBCSMTTransaction
    , newRocksDBState
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns
    , Prisms
    , codecs
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    , RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( PartialHistory
    , UpdateTrace
    , newState
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Query
    , Update
    )
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
    ( ReaderT (..)
    )
import Control.Tracer (Tracer)
import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
import Database.KV.Transaction qualified as L
import Database.RocksDB (BatchOp, ColumnFamily, DB (..))

type RocksDBTransaction m slot hash key value =
    L.Transaction m ColumnFamily (Columns slot hash key value) BatchOp

type RocksDBQuery m slot hash key value =
    Query m slot key value

-- | Create a 'RunTransaction' for RocksDB
newRunRocksDBTransaction
    :: (MonadIO m, MonadFail m, MonadMask m)
    => DB
    -> Prisms slot hash key value
    -- ^ Prisms for serializing/deserializing keys and values
    -> m (RunTransaction ColumnFamily BatchOp slot hash key value m)
newRunRocksDBTransaction db prisms = do
    L.RunTransaction rt <- newRunTransaction db prisms
    pure $ RunTransaction rt

newRunRocksDBCSMTTransaction
    :: (MonadIO m, MonadFail m, MonadMask m)
    => DB
    -> Prisms slot hash key value
    -- ^ Prisms for serializing/deserializing keys and values
    -> CSMTContext hash key value
    -> m
        ( RunCSMTTransaction
            ColumnFamily
            BatchOp
            slot
            hash
            key
            value
            m
        )
newRunRocksDBCSMTTransaction db prisms csmtContext = do
    L.RunTransaction rt <- newRunTransaction db prisms
    pure
        $ RunCSMTTransaction
        $ \tx -> runReaderT (rt tx) csmtContext

newRunTransaction
    :: (MonadIO m, MonadIO n, MonadMask n, MonadFail n)
    => DB
    -> Prisms slot hash key value
    -> m
        ( L.RunTransaction
            n
            ColumnFamily
            (Columns slot hash key value)
            BatchOp
        )
newRunTransaction db prisms =
    L.newRunTransaction
        $ mkRocksDBDatabase db
        $ mkColumns (columnFamilies db)
        $ codecs prisms

newRocksDBState
    :: (MonadIO m, MonadFail m, Ord key, Ord slot, MonadMask m)
    => Tracer m (UpdateTrace slot hash)
    -> DB
    -> Prisms slot hash key value
    -> CSMTContext hash key value
    -> PartialHistory
    -> (slot -> hash)
    -> ArmageddonParams hash
    -> m
        ( (Update m slot key value, [slot])
        , RunCSMTTransaction ColumnFamily BatchOp slot hash key value m
        )
newRocksDBState tracer db prisms csmtContext partiality slotHash armageddonParams = do
    runner <- newRunRocksDBCSMTTransaction db prisms csmtContext
    (,runner)
        <$> newState tracer partiality slotHash armageddonParams runner
