module Cardano.N2N.Client.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , armageddon
    )
where

import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction (..)
    )
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Data.Function (fix)
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Transaction
    ( KeyOf
    , delete
    , iterating
    )

-- | Parameters for performing an "armageddon" cleanup of the database
newtype ArmageddonParams = ArmageddonParams
    { armageddonBatchSize :: Int
    -- ^ Number of entries to delete per batch
    }

-- Clean up a column batch of rows
cleanUpBatch
    :: (Ord (KeyOf x), Monad m)
    => RunCSMTTransaction cf op slot hash key value m
    -> Columns slot hash key value x
    -> ArmageddonParams
    -> m ()
cleanUpBatch
    RunCSMTTransaction{txRunTransaction = transact}
    column
    ArmageddonParams{armageddonBatchSize} = do
        fix $ \batch -> do
            r <- transact $ iterating column $ do
                me <- firstEntry
                ($ (me, 0)) $ fix $ \go -> \case
                    (Nothing, _) -> pure False
                    (_, m) | m >= armageddonBatchSize -> pure True
                    (Just Entry{entryKey}, count) -> do
                        lift $ delete column entryKey
                        next <- nextEntry
                        go (next, count + 1)
            when r batch

-- | Perform an "armageddon" cleanup of the database
-- by deleting all entries in all columns in batches
-- THIS IS NOT GOING TO RUN ATOMICALLY
armageddon
    :: (Ord key, Ord slot, Monad m)
    => RunCSMTTransaction cf op slot hash key value m
    -> ArmageddonParams
    -> m ()
armageddon runTransaction armageddonParams = do
    cleanUpBatch runTransaction KVCol armageddonParams
    cleanUpBatch runTransaction CSMTCol armageddonParams
    cleanUpBatch runTransaction RollbackPoints armageddonParams
