module Cardano.N2N.Client.Application.Database.InMemory (InMemory (..), mkInMemoryDatabaseSimple)
where

import Cardano.N2N.Client.Application.Database.Interface
    ( Database (..)
    , Event (..)
    , Operation (..)
    , ProgressK (..)
    , Update (..)
    , UpdateBox (..)
    , UpdateResult (..)
    , inverseOp
    )
import Control.Monad (forM)
import Control.Monad.State.Strict (MonadState (..), State, modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Ouroboros.Network.Point (WithOrigin (..))

data InMemory slot key value = InMemory
    { mUTxos :: Map key value
    , mInverseOps :: Map slot [Operation key value]
    , mTip :: WithOrigin slot
    , mFinality :: WithOrigin slot
    }

emptyInMemory :: (Ord key, Ord slot) => InMemory slot key value
emptyInMemory =
    InMemory
        { mUTxos = mempty
        , mInverseOps = mempty
        , mTip = Origin
        , mFinality = Origin
        }
type InMemoryState slot key value = State (InMemory slot key value)

mkInMemoryDatabaseSimple
    :: (Ord key, Ord slot)
    => Database (InMemoryState slot key value) slot key value
mkInMemoryDatabaseSimple =
    Database
        { update = updateInMemory getValue
        , getValue
        , getTip
        , getFinality
        }
  where
    getValue key = do
        InMemory{mUTxos} <- get
        pure $ Map.lookup key mUTxos
    getTip = do
        InMemory{mTip} <- get
        pure mTip
    getFinality = do
        InMemory{mFinality} <- get
        pure mFinality

updateInMemory
    :: (Ord key, Ord slot)
    => (key -> InMemoryState slot key value (Maybe value))
    -> Update 'ProgressT (State (InMemory slot key value)) slot key value
updateInMemory value = TakeEvent $ \case
    ForwardTip slot ops -> do
        inverseOps <- forM ops (inverseOp value)
        modify $ \im@InMemory{mUTxos, mInverseOps} ->
            let mUTxos' = foldr applyOp mUTxos ops
                mInverseOps' = Map.insert slot inverseOps mInverseOps
            in  im{mUTxos = mUTxos', mInverseOps = mInverseOps', mTip = At slot}
        pure $ UpdateBox $ Progress $ updateInMemory value
    RollbackTip Origin -> do
        put emptyInMemory
        pure $ UpdateBox $ Progress $ updateInMemory value
    RollbackTip (At point) -> do
        im@InMemory{mUTxos, mInverseOps, mTip} <- get

        if At point < mTip
            then pure $ UpdateBox $ Rewind $ Truncate $ do
                put emptyInMemory
                pure $ Progress $ updateInMemory value
            else do
                let (toBeInverted, mInverseOps') =
                        Map.split point mInverseOps
                    opsToInvert = concatMap snd $ Map.toAscList toBeInverted
                    mUTxos' = foldr applyOp mUTxos opsToInvert
                put im{mUTxos = mUTxos', mInverseOps = mInverseOps', mTip = At point}
                pure $ UpdateBox $ Progress $ updateInMemory value
    ForwardFinality slot -> do
        modify $ \im@InMemory{mFinality, mTip, mInverseOps} ->
            let mFinalityPrime' = min (max mFinality (At slot)) mTip
                (mInverseOps', _) = Map.split slot mInverseOps
            in  im{mFinality = mFinalityPrime', mInverseOps = mInverseOps'}
        pure $ UpdateBox $ Progress $ updateInMemory value

applyOp
    :: Ord key => Operation key value -> Map key value -> Map key value
applyOp op m = case op of
    Insert k v -> Map.insert k v m
    Delete k -> Map.delete k m
