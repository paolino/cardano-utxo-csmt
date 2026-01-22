module Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , ArmageddonTrace (..)
    , armageddon
    , setup
    , renderArmageddonTrace
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunCSMTTransaction (..)
    )
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Transaction
    ( KeyOf
    , delete
    , insert
    , iterating
    )
import Ouroboros.Network.Point (WithOrigin (..))

data ArmageddonTrace
    = ArmageddonStarted
    | ArmageddonCompleted
    | SetupDone

renderArmageddonTrace :: ArmageddonTrace -> String
renderArmageddonTrace ArmageddonStarted = " Armageddon cleanup started."
renderArmageddonTrace ArmageddonCompleted = " Armageddon cleanup completed."
renderArmageddonTrace SetupDone = " Initial setup done."

-- | Parameters for performing an "armageddon" cleanup of the database
data ArmageddonParams hash = ArmageddonParams
    { armageddonBatchSize :: Int
    -- ^ Number of entries to delete per batch
    , noHash :: hash
    }

-- Clean up a column batch of rows
cleanUpBatch
    :: (Ord (KeyOf x), Monad m)
    => RunCSMTTransaction cf op slot hash key value m
    -> Columns slot hash key value x
    -> ArmageddonParams hash
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
    => Tracer m ArmageddonTrace
    -> RunCSMTTransaction cf op slot hash key value m
    -> ArmageddonParams hash
    -> m ()
armageddon tracer@(traceWith -> trace) runTransaction armageddonParams = do
    trace ArmageddonStarted
    cleanUpBatch runTransaction KVCol armageddonParams
    cleanUpBatch runTransaction CSMTCol armageddonParams
    cleanUpBatch runTransaction RollbackPoints armageddonParams
    setup tracer runTransaction armageddonParams
    trace ArmageddonCompleted

setup
    :: (Ord slot, Monad m)
    => Tracer m ArmageddonTrace
    -> RunCSMTTransaction cf op slot hash key value m
    -> ArmageddonParams hash
    -> m ()
setup (traceWith -> trace) (RunCSMTTransaction{txRunTransaction}) armageddonParams = do
    txRunTransaction
        $ insert
            RollbackPoints
            Origin
            ( RollbackPoint
                { rbpHash = noHash armageddonParams
                , rbpInverseOperations = []
                , rpbMerkleRoot = Nothing
                }
            )
    trace SetupDone
