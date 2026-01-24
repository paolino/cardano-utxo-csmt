{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Cardano.UTxOCSMT.Application.BlockFetch
Description : Block fetching client for Cardano node

This module implements the BlockFetch mini-protocol client that retrieves
full block data from the Cardano node. It works in coordination with
the ChainSync client:

1. ChainSync receives headers and queues them
2. BlockFetch retrieves full blocks for queued headers
3. Fetched blocks are passed to the database update layer

The bounded queue prevents memory exhaustion during fast sync periods.
-}
module Cardano.UTxOCSMT.Application.BlockFetch
    ( mkBlockFetchApplication
    , Fetched (..)
    , EventQueueLength (..)
    , BlockFetchApplication
    )
where

import Cardano.UTxOCSMT.Application.ChainSync
    ( Follower (..)
    , ProgressOrRewind (..)
    )
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Block
    , BlockFetchApplication
    , Header
    , Intersector (..)
    , Point
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , flushTBQueue
    , isEmptyTBQueue
    , newTBQueueIO
    , writeTBQueue
    )
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Lens (makeWrapped)
import Control.Monad (unless)
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block (blockPoint)
import Ouroboros.Network.BlockFetch.ClientState (ChainRange (..))
import Ouroboros.Network.Protocol.BlockFetch.Client
    ( BlockFetchClient (..)
    , BlockFetchReceiver (..)
    , BlockFetchRequest (..)
    , BlockFetchResponse (..)
    )

-- | Length of the queue of headers to fetch blocks for
newtype EventQueueLength = EventQueueLength Int
    deriving (Show)

makeWrapped ''EventQueueLength

-- | A fetched block along with its point
data Fetched = Fetched
    { fetchedPoint :: Point
    , fetchedBlock :: Block
    }

{- | Create a block fetch application and promote compute an header intersector
for the chain sync application
-}
mkBlockFetchApplication
    :: EventQueueLength
    -- ^ maximum length of the headers queue
    -> Tracer IO EventQueueLength
    -- ^ metrics tracer
    -> Intersector Fetched
    -- ^ callback to process each fetched block
    -> IO (BlockFetchApplication, Intersector Header)
mkBlockFetchApplication (EventQueueLength maxQueueLen) tr blockIntersector = do
    queue <- newQueue maxQueueLen
    blockFollowerVar <- newEmptyMVar
    pure
        ( blockFetchClient tr blockFollowerVar queue
        , mkHeaderIntersector blockFollowerVar queue blockIntersector
        )

headerFollower
    :: MVar (Follower Fetched)
    -> Queue Point
    -> Follower Header
headerFollower blockFollowerVar queue@Queue{pushQueue, waitEmptyQueue} = fix $ \go ->
    Follower
        { rollForward = \header -> do
            -- we do not wait if there is space in the queue
            pushQueue $ blockPoint header
            pure go
        , rollBackward = \point -> do
            waitEmptyQueue
            -- we are safe as the only source of headers is rollForward above
            -- and it's alternative to rollBackward
            Follower{rollBackward} <- takeMVar blockFollowerVar
            progressOrRewind <- rollBackward point
            case progressOrRewind of
                Progress blockFollower -> do
                    putMVar blockFollowerVar blockFollower
                    pure $ Progress go
                Rewind points blockIntersector -> do
                    pure
                        $ Rewind points
                        $ mkHeaderIntersector
                            blockFollowerVar
                            queue
                            blockIntersector
                Reset blockIntersector -> do
                    pure
                        $ Reset
                        $ mkHeaderIntersector
                            blockFollowerVar
                            queue
                            blockIntersector
        }

mkHeaderIntersector
    :: MVar (Follower Fetched)
    -> Queue Point
    -> Intersector Fetched
    -> Intersector Header
mkHeaderIntersector blockFollowerVar q = fix $ \go blockIntersector ->
    Intersector
        { intersectFound = \point -> do
            blockFollower <- intersectFound blockIntersector point
            putMVar blockFollowerVar blockFollower
            pure $ headerFollower blockFollowerVar q
        , intersectNotFound = do
            (blockIntersector', points) <- intersectNotFound blockIntersector
            pure (go blockIntersector', points)
        }

blockFetchReceiver
    :: MVar (Follower Fetched)
    -> NonEmpty Point
    -> IO (BlockFetchReceiver Block IO)
blockFetchReceiver blockFollowerVar points = do
    blockFollower <- takeMVar blockFollowerVar
    pure
        $ ($ NE.toList points)
        $ ($ blockFollower)
        $ fix
        $ \fetchOne bf ps ->
            BlockFetchReceiver
                { handleBlock = \block -> do
                    case ps of
                        [] ->
                            error
                                "mkBlockFetchApplication: \
                                \more blocks fetched than requested"
                        p : ps' -> do
                            bf' <-
                                rollForward
                                    bf
                                    Fetched
                                        { fetchedPoint = p
                                        , fetchedBlock = block
                                        }
                            pure $ fetchOne bf' ps'
                , handleBatchDone = putMVar blockFollowerVar bf
                }

blockFetchClient
    :: Tracer IO EventQueueLength
    -> MVar (Follower Fetched)
    -> Queue Point
    -> BlockFetchClient Block Point IO ()
blockFetchClient tracer blockFollowerVar Queue{flushQueue} = fix $ \go ->
    BlockFetchClient $ do
        points <- flushQueue
        traceWith tracer $ EventQueueLength $ length points
        pure
            $ SendMsgRequestRange
                (mkRange points)
                BlockFetchResponse
                    { handleStartBatch = blockFetchReceiver blockFollowerVar points
                    , handleNoBlocks = error "blockFetchClient: no blocks to fetch"
                    }
                go

mkRange :: NonEmpty a -> ChainRange a
mkRange xs@(x :| _) = ChainRange x (NE.last xs)

retryNull
    :: MonadSTM m
    => [a]
    -> STM m (NonEmpty a)
retryNull [] = retry
retryNull (y : ys) = pure (y :| ys)

-- | A simple bounded queue
data Queue a = Queue
    { pushQueue :: a -> IO ()
    , flushQueue :: IO (NonEmpty a)
    , waitEmptyQueue :: IO ()
    }

newQueue
    :: Int
    -> IO (Queue a)
newQueue size = do
    queue <- newTBQueueIO $ fromIntegral size
    let push = writeTBQueue queue
        flush = flushTBQueue queue >>= retryNull
        waitEmpty = do
            isEmpty <- isEmptyTBQueue queue
            unless isEmpty retry
    pure
        Queue
            { pushQueue = atomically <$> push
            , flushQueue = atomically flush
            , waitEmptyQueue = atomically waitEmpty
            }
