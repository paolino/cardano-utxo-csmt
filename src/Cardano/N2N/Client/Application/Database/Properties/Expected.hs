{-# LANGUAGE ConstraintKinds #-}

{-
This module exposes an API to express properties about a database.

It provides a monad transformer stack 'WithExpected' that combines a
'ReaderT' for accessing the database and generator context, and a 'StateT'
for maintaining the expected state of the database.

The expected state is represented by the 'Expected' data type, which tracks
the expected key-value associations, the tip and immutable slots, and the
opposite operations for rollbacks.

-}
module Cardano.N2N.Client.Application.Database.Properties.Expected
    ( Expected (..)
    , WithExpected
    , PropertyWithExpected
    , PropertyConstraints
    , Generator (..)
    , Context (..)
    , expectedForward
    , runDb
    , forwardTip
    , expectedNewestSlot
    , asksGenerator
    , getDump
    , expectedKeys
    , expectedOpposite
    , getTip
    , getFinality
    , emptyExpected
    , rollbackTip
    , forwardFinality
    , expectedAllOpposites
    , runWithExpected
    )
where

import Cardano.N2N.Client.Application.Database.Interface
    ( Database
    , Dump (..)
    , Event (..)
    , Operation (..)
    , ProgressK (..)
    , Update (..)
    , UpdateBox (..)
    , UpdateResult (..)
    , dumpDatabase
    )
import Cardano.N2N.Client.Application.Database.Interface qualified as Interface
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State
    ( MonadState (..)
    , MonadTrans (..)
    , StateT
    , evalStateT
    , gets
    )
import Data.Foldable (Foldable (..))
import GHC.Stack (HasCallStack)
import Ouroboros.Network.Point (WithOrigin (..))
import Test.QuickCheck (Gen)
import Test.QuickCheck.Monadic (PropertyM, run)

-- | Constraints required for properties
type PropertyConstraints m slot key value =
    ( Eq value
    , Show slot
    , Show key
    , Show value
    , Ord slot
    , Ord key
    , Monad m
    )

-- | Generator for slots, keys, and values
data Generator slot key value = Generator
    { genSlot :: Gen slot
    , genKey :: Gen key
    , genValue :: Gen value
    }

-- | Context for properties
data Context m slot key value = Context
    { contextDatabase :: Database m slot key value
    , contextGenerator :: Generator slot key value
    }

-- | Expected state of the database
data Expected slot key value = Expected
    { expectedAssocs :: [(key, value)]
    , expectedTip :: WithOrigin slot
    , expectedFinality :: WithOrigin slot
    , expectedOpposites :: [(slot, [Operation key value])]
    }
    deriving (Show, Eq)

emptyExpected :: Expected slot key value
emptyExpected =
    Expected
        { expectedAssocs = []
        , expectedTip = Origin
        , expectedFinality = Origin
        , expectedOpposites = []
        }

-- | Monad transformer stack for properties with expected state
type WithExpected m slot key value =
    ReaderT
        (Context m slot key value)
        (TrackingExpected m slot key value)

type TrackingExpected m slot key value =
    StateT (Expected slot key value, UpdateBox m slot key value) m

runWithExpected
    :: PropertyConstraints m slot key value
    => Context m slot key value
    -> UpdateBox m slot key value
    -> WithExpected m slot key value a
    -> m a
runWithExpected context box prop =
    flip evalStateT (emptyExpected, box)
        $ runReaderT prop context

-- | Access the generator from the context
asksGenerator
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value (Generator slot key value)
asksGenerator = lift $ asks contextGenerator

-- | Property monad with expected state
type PropertyWithExpected m slot key value =
    PropertyM (WithExpected m slot key value)

runDb
    :: Monad m
    => (Database m slot key value -> m a)
    -> PropertyWithExpected m slot key value a
runDb f = run $ do
    db <- asks contextDatabase
    lift . lift $ f db

-- | Proxy to database 'dumpDatabase'
getDump
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value (Dump slot key value)
getDump = do
    keys <- lift . lift $ gets $ expectedAssocs . fst
    runDb $ dumpDatabase $ fmap fst keys

-- | Provide an expected opposite operation
expectedOpposite
    :: (Eq key, Monad m, HasCallStack)
    => Operation key value
    -> PropertyWithExpected m slot key value (Operation key value)
expectedOpposite op = lift . lift $ gets $ \(expct, _) -> opposite expct op

expectedAllOpposites
    :: Monad m
    => PropertyWithExpected m slot key value [Operation key value]
expectedAllOpposites = do
    opps <- lift . lift $ gets $ \(expct, _) -> expectedOpposites expct
    pure $ concatMap snd opps

-- | Proxy to database 'getTip'
getTip
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value (WithOrigin slot)
getTip = runDb $ \db -> Interface.getTip db

-- | Proxy to database 'getFinality'
getFinality
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value (WithOrigin slot)
getFinality = runDb $ \db -> Interface.getFinality db

-- | Get all expected keys
expectedKeys :: Expected slot key value -> [key]
expectedKeys = fmap fst . expectedAssocs

-- | Get the newest expected slot
expectedNewestSlot :: Expected slot key value -> WithOrigin slot
expectedNewestSlot Expected{expectedOpposites} =
    case expectedOpposites of
        [] -> Origin
        (x : _) -> At . fst $ x

applyOperation :: Eq a => [(a, b)] -> Operation a b -> [(a, b)]
applyOperation expct (Insert k v) = (k, v) : expct
applyOperation expct (Delete k) = filter (\(k', _) -> k' /= k) expct

applyOperations :: Eq a => [(a, b)] -> [Operation a b] -> [(a, b)]
applyOperations = foldl' applyOperation

applyOpposite
    :: Eq slot
    => [(slot, [Operation key value])]
    -> slot
    -> Operation key value
    -> [(slot, [Operation key value])]
applyOpposite [] slot op = [(slot, [op])]
applyOpposite ((slt, ops) : rest) slot op
    | slt == slot = (slt, op : ops) : rest
    | otherwise = (slot, [op]) : (slt, ops) : rest

opposite
    :: (Eq key, HasCallStack)
    => Expected slot key value
    -> Operation key value
    -> Operation key value
opposite _ (Insert k _) = Delete k
opposite expct (Delete k) =
    case lookup k (expectedAssocs expct) of
        Just v -> Insert k v
        Nothing -> error "opposite: key not found in expected contents"

expectedForward
    :: (Eq key, Ord slot, HasCallStack)
    => Expected slot key value
    -> slot
    -> [Operation key value]
    -> Expected slot key value
expectedForward old@Expected{expectedTip} slot ops
    | At slot <= expectedTip = old
    | otherwise = foldl' apply old{expectedTip = At slot} ops
  where
    apply ex op =
        ex
            { expectedAssocs = applyOperation (expectedAssocs ex) op
            , expectedOpposites =
                applyOpposite (expectedOpposites ex) slot (opposite ex op)
            }

-- | Proxy to database 'forward' that also updates expected state
forwardTip
    :: forall m slot key value
     . (PropertyConstraints m slot key value, HasCallStack)
    => slot
    -> [Operation key value]
    -> PropertyWithExpected m slot key value ()
forwardTip slot ops = lift . lift $ do
    (ex, UpdateBox next) <- get
    truncating next $ \case
        TakeEvent update -> do
            cont <- lift $ update (ForwardTip slot ops)
            put (expectedForward ex slot ops, cont)

expectedRollback
    :: (Eq key, Ord slot)
    => Expected slot key value
    -> WithOrigin slot
    -> Expected slot key value
expectedRollback
    old@Expected
        { expectedAssocs = assocs
        , expectedOpposites = opposites
        , expectedFinality = expectedFinality
        , expectedTip
        }
    (At slot)
        | At slot >= expectedTip = old
        | At slot < expectedTip && At slot >= expectedFinality =
            let
                (ops, newOpposites) = break (\(slt, _) -> slt <= slot) opposites
                newAssocs = applyOperations assocs $ ops >>= snd
            in
                Expected
                    { expectedAssocs = newAssocs
                    , expectedTip = At slot
                    , expectedFinality
                    , expectedOpposites = newOpposites
                    }
        | otherwise = emptyExpected
expectedRollback _ Origin =
    emptyExpected

-- | Proxy to database 'rollback' that also updates expected state
rollbackTip
    :: forall m slot key value
     . PropertyConstraints m slot key value
    => WithOrigin slot
    -> PropertyWithExpected m slot key value ()
rollbackTip newTip = lift . lift $ do
    (ex, UpdateBox next) <- get
    truncating next $ \case
        TakeEvent update -> do
            cont <- lift $ update (RollbackTip newTip)
            put (expectedRollback ex newTip, cont)

expectedProgressFinality
    :: Ord slot
    => Expected slot key value
    -> slot
    -> Expected slot key value
expectedProgressFinality
    old@Expected{expectedFinality, expectedTip, expectedOpposites}
    slot
        | At slot <= expectedFinality = old
        | otherwise =
            let
                slot' = min (At slot) expectedTip
            in
                old
                    { expectedFinality = slot'
                    , expectedOpposites =
                        takeWhile (\(slt, _) -> At slt > slot') expectedOpposites
                    }

-- | Proxy to database 'progressFinality' that also updates expected state
forwardFinality
    :: forall m slot key value
     . PropertyConstraints m slot key value
    => slot
    -> PropertyWithExpected m slot key value ()
forwardFinality slot = lift . lift $ do
    (ex, UpdateBox next) <- get
    truncating next $ \case
        TakeEvent update -> do
            cont <- lift $ update (ForwardFinality slot)
            put (expectedProgressFinality ex slot, cont)

truncating
    :: Monad m
    => UpdateResult p m slot key value
    -> ( Update 'ProgressT m slot key value
         -> TrackingExpected m slot key value a
       )
    -> TrackingExpected m slot key value a
truncating (Rewind (Truncate truncateDB)) p = do
    pe <- lift truncateDB
    case pe of
        Progress e -> p e
truncating (Progress e) p = p e
