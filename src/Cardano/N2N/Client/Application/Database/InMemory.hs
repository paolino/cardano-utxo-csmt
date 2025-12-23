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
    , Event (..)
    , Operation (..)
    , ProgressK (..)
    , Update (..)
    , UpdateBox (..)
    , UpdateResult (..)
    , inverseOp
    )
import Control.Monad (forM_, when)
import Control.Monad.State.Strict
    ( MonadState (..)
    , State
    , evalState
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
type InMemoryState slot key value = State (InMemory slot key value)

runInMemoryState
    :: (Ord key, Ord slot) => InMemoryState slot key value a -> a
runInMemoryState st = evalState st emptyInMemory

mkInMemoryDatabaseSimple
    :: Ord key
    => Database (InMemoryState slot key value) slot key value
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
    :: (Ord key, Ord slot)
    => Database (InMemoryState slot key value) slot key value
    -> Update 'ProgressT (State (InMemory slot key value)) slot key value
updateInMemory value = TakeEvent $ \case
    ForwardTip slot ops -> do
        tip <- getTip value
        when (At slot > tip) $ do
            forM_ ops $ \op -> do
                opI <- inverseOp (getValue value) op
                modify $ \im@InMemory{mUTxos, mInverseOps} ->
                    let mUTxos' = applyOp mUTxos op
                        mInverseOps' = Map.insertWith (<>) slot [opI] mInverseOps
                    in  im{mUTxos = mUTxos', mInverseOps = mInverseOps', mTip = At slot}
        pure $ UpdateBox $ Progress $ updateInMemory value
    RollbackTip Origin -> do
        put emptyInMemory
        pure $ UpdateBox $ Progress $ updateInMemory value
    RollbackTip (At point) -> do
        im@InMemory{mUTxos, mInverseOps, mTip, mFinality} <- get
        if At point < mFinality
            then do
                put emptyInMemory
                pure $ UpdateBox $ Rewind $ Truncate $ do
                    pure $ Progress $ updateInMemory value
            else do
                when (At point < mTip) $ do
                    let (mInverseOps', toBeInverted) = Map.split point mInverseOps
                        opsToInvert = concatMap snd $ Map.toDescList toBeInverted
                        mUTxos' = foldl' applyOp mUTxos opsToInvert
                    put im{mUTxos = mUTxos', mInverseOps = mInverseOps', mTip = At point}
                pure $ UpdateBox $ Progress $ updateInMemory value
    ForwardFinality slot -> do
        modify $ \im@InMemory{mFinality, mTip, mInverseOps} ->
            let mFinalityPrime' = min (max mFinality (At slot)) mTip
                (_, mInverseOps') = Map.split slot mInverseOps
            in  im{mFinality = mFinalityPrime', mInverseOps = mInverseOps'}
        pure $ UpdateBox $ Progress $ updateInMemory value

applyOp
    :: Ord key => Map key value -> Operation key value -> Map key value
applyOp m op = case op of
    Insert k v -> Map.insert k v m
    Delete k -> Map.delete k m
