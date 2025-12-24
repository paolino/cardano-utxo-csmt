module Cardano.N2N.Client.Application.Database.InMemory
    ( InMemory (..)
    , InMemoryState
    , emptyInMemory
    , mkInMemoryDatabaseSimple
    , runInMemoryState
    , updateInMemory
    )
where

import Cardano.N2N.Client.Application.Database.Interface
    ( Database (..)
    , Operation (..)
    , Truncated (..)
    , Update (..)
    , inverseOp
    )
import Control.Monad (forM_, when)
import Control.Monad.State.Strict
    ( MonadState (..)
    , StateT
    , evalStateT
    , modify
    )
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Ouroboros.Network.Point (WithOrigin (..))

data InMemory slot key value = InMemory
    { mUTxos :: Map key value
    , mInverseOps :: Map slot [Operation key value]
    , mTip :: WithOrigin slot
    , mFinality :: WithOrigin slot
    }
    deriving (Show, Eq)

emptyInMemory :: (Ord key, Ord slot) => InMemory slot key value
emptyInMemory =
    InMemory
        { mUTxos = mempty
        , mInverseOps = mempty
        , mTip = Origin
        , mFinality = Origin
        }
type InMemoryState m slot key value =
    StateT (InMemory slot key value) m

runInMemoryState
    :: (Ord key, Ord slot, Monad m)
    => InMemoryState m slot key value a
    -> m a
runInMemoryState st = evalStateT st emptyInMemory

mkInMemoryDatabaseSimple
    :: (Ord key, Monad m)
    => Database (InMemoryState m slot key value) slot key value
mkInMemoryDatabaseSimple =
    Database
        { getValue
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
    :: (Ord key, Ord slot, Monad m)
    => Database (InMemoryState m slot key value) slot key value
    -> Update (InMemoryState m slot key value) slot key value
updateInMemory value =
    Update
        { forwardTipApply = \slot ops -> do
            tip <- getTip value
            when (At slot > tip) $ do
                forM_ ops $ \op -> do
                    opI <- inverseOp (getValue value) op
                    modify $ \im@InMemory{mUTxos, mInverseOps} ->
                        let mUTxos' = applyOp mUTxos op
                            mInverseOps' = Map.insertWith (<>) slot [opI] mInverseOps
                        in  im{mUTxos = mUTxos', mInverseOps = mInverseOps', mTip = At slot}
            pure $ updateInMemory value
        , rollbackTipApply = \case
            Origin -> do
                put emptyInMemory
                pure $ NotTruncated $ updateInMemory value
            (At point) -> do
                im@InMemory{mUTxos, mInverseOps, mTip, mFinality} <- get
                if At point < mFinality
                    then do
                        put emptyInMemory
                        pure $ Truncated $ updateInMemory value
                    else do
                        when (At point < mTip) $ do
                            let (mInverseOps', toBeInverted) = Map.split point mInverseOps
                                opsToInvert = concatMap snd $ Map.toDescList toBeInverted
                                mUTxos' = foldl' applyOp mUTxos opsToInvert
                            put im{mUTxos = mUTxos', mInverseOps = mInverseOps', mTip = At point}
                        pure $ NotTruncated $ updateInMemory value
        , forwardFinalityApply = \slot -> do
            modify $ \im@InMemory{mFinality, mTip, mInverseOps} ->
                let mFinalityPrime' = min (max mFinality (At slot)) mTip
                    (_, mInverseOps') = Map.split slot mInverseOps
                in  im{mFinality = mFinalityPrime', mInverseOps = mInverseOps'}
            pure $ updateInMemory value
        }

applyOp
    :: Ord key => Map key value -> Operation key value -> Map key value
applyOp m op = case op of
    Insert k v -> Map.insert k v m
    Delete k -> Map.delete k m
