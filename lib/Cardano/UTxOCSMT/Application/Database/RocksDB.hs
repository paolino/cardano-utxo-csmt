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
    , createUpdateState
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms
    , codecs
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    , RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( UpdateTrace
    , newState
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Query
    , TipOf
    , Update
    )
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
    ( ReaderT (..)
    )
import Control.Tracer (Tracer, nullTracer)
import Data.Maybe (isNothing)
import Database.KV.Cursor (firstEntry)
import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
import Database.KV.Transaction (iterating)
import Database.KV.Transaction qualified as L
import Database.RocksDB (BatchOp, ColumnFamily, DB (..))
import UnliftIO (MonadUnliftIO)

type RocksDBTransaction m slot hash key value =
    L.Transaction m ColumnFamily (Columns slot hash key value) BatchOp

type RocksDBQuery m slot hash key value =
    Query m slot key value

-- | Create a 'RunTransaction' for RocksDB
newRunRocksDBTransaction
    :: (MonadUnliftIO m, MonadFail m, MonadMask m)
    => DB
    -> Prisms slot hash key value
    -- ^ Prisms for serializing/deserializing keys and values
    -> m (RunTransaction ColumnFamily BatchOp slot hash key value m)
newRunRocksDBTransaction db prisms = do
    L.RunTransaction rt <- newRunTransaction db prisms
    pure $ RunTransaction rt

newRunRocksDBCSMTTransaction
    :: (MonadUnliftIO m, MonadFail m, MonadMask m)
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
    :: (MonadIO m, MonadUnliftIO n, MonadMask n, MonadFail n)
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
    :: (MonadUnliftIO m, MonadFail m, Ord key, Ord slot, MonadMask m)
    => Tracer m (UpdateTrace slot hash)
    -> DB
    -> Prisms slot hash key value
    -> CSMTContext hash key value
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -- ^ Called after each forward; use to check if at tip and emit Synced
    -> ArmageddonParams hash
    -> m
        ( (Update m slot key value, [slot])
        , RunCSMTTransaction ColumnFamily BatchOp slot hash key value m
        )
newRocksDBState
    tracer
    db
    prisms
    csmtContext
    slotHash
    onForward
    armageddonParams = do
        runner <- newRunRocksDBCSMTTransaction db prisms csmtContext
        ensureInitialized runner armageddonParams
        (,runner)
            <$> newState tracer slotHash onForward armageddonParams runner

-- | Create Update state from an existing runner
createUpdateState
    :: (MonadFail m, Ord key, Ord slot)
    => Tracer m (UpdateTrace slot hash)
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -- ^ Called after each forward; use to check if at tip and emit Synced
    -> ArmageddonParams hash
    -> RunCSMTTransaction ColumnFamily BatchOp slot hash key value m
    -> m (Update m slot key value, [slot])
createUpdateState tracer slotHash onForward armageddonParams runner = do
    ensureInitialized runner armageddonParams
    newState tracer slotHash onForward armageddonParams runner

{- | Ensure the database has been initialized with an Origin rollback point.
This makes the public API self-initializing so callers can't forget to
call 'setup' before creating state.
-}
ensureInitialized
    :: (Ord slot, Monad m)
    => RunCSMTTransaction cf op slot hash key value m
    -> ArmageddonParams hash
    -> m ()
ensureInitialized runner@RunCSMTTransaction{txRunTransaction} armageddonParams = do
    empty <-
        txRunTransaction $ iterating RollbackPoints $ isNothing <$> firstEntry
    when empty $ setup nullTracer runner armageddonParams
