{-# LANGUAGE TemplateHaskell #-}

module Cardano.N2N.Client.Application.BlockFetch
    ( mkBlockFetchApplication
    , EventQueueLength (..)
    , BlockFetchApplication
    )
where

import Cardano.N2N.Client.Application.ChainSync
    ( Follower (..)
    , ProgressOrRewind (..)
    )
import Cardano.N2N.Client.Ouroboros.Types
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
    ( modifyMVar_
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Lens (makeWrapped)
import Control.Monad (unless)
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Data.IORef (newIORef, readIORef, writeIORef)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block (blockPoint)
import Ouroboros.Network.BlockFetch.ClientState (ChainRange (..))
import Ouroboros.Network.Protocol.BlockFetch.Client
    ( BlockFetchClient (..)
    , BlockFetchReceiver (..)
    , BlockFetchRequest (..)
    , BlockFetchResponse (..)
    )

newtype EventQueueLength = EventQueueLength Int
    deriving (Show)

makeWrapped ''EventQueueLength

mkBlockFetchApplication
    :: EventQueueLength
    -- ^ max length of the event queue
    -> Tracer IO EventQueueLength
    -- ^ metrics tracer
    -> Intersector (Point, Block)
    -- ^ callback to process each fetched block
    -> IO (BlockFetchApplication, Intersector Header)
mkBlockFetchApplication (EventQueueLength maxQueueLen) tr blockIntersector = do
    (pushHeader, flushHeaders, waitEmpty) <- queue maxQueueLen
    blockFollowerVar <- newEmptyMVar
    let mkHeaderIntersector blockIntersector' =
            Intersector
                { intersectFound = \point -> do
                    blockFollower' <- intersectFound blockIntersector' point
                    putMVar blockFollowerVar blockFollower'
                    pure headerFollower
                , intersectNotFound = do
                    (blockIntersector'', points) <- intersectNotFound blockIntersector'
                    pure (mkHeaderIntersector blockIntersector'', points)
                }
        headerFollower =
            Follower
                { rollForward = \header -> do
                    pushHeader $ blockPoint header
                    pure headerFollower
                , rollBackward = \point -> do
                    waitEmpty
                    -- we are safe as the only source of headers is rollForward above
                    Follower{rollBackward} <- takeMVar blockFollowerVar
                    fr <- rollBackward point
                    case fr of
                        Progress blockFollower' -> do
                            putMVar blockFollowerVar blockFollower'
                            pure (Progress headerFollower)
                        Rewind points blockIntersector' -> do
                            pure
                                $ Rewind points
                                $ mkHeaderIntersector blockIntersector'
                        Reset blockFollower' -> do
                            putMVar blockFollowerVar blockFollower'
                            pure (Reset headerFollower)
                }
        blockFetchClient = BlockFetchClient $ do
            (range, points, len) <- flushHeaders
            traceWith tr len
            pointsVar <- newIORef points
            pure
                $ SendMsgRequestRange
                    range
                    BlockFetchResponse
                        { handleStartBatch = pure
                            $ fix
                            $ \fetchOne ->
                                BlockFetchReceiver
                                    { handleBlock = \block -> do
                                        pointsLeft <- readIORef pointsVar
                                        case pointsLeft of
                                            [] ->
                                                error
                                                    "mkBlockFetchApplication: \
                                                    \more blocks fetched than requested"
                                            p : ps -> do
                                                writeIORef pointsVar ps
                                                modifyMVar_ blockFollowerVar
                                                    $ flip rollForward (p, block)
                                        pure fetchOne
                                    , handleBatchDone = pure ()
                                    }
                        , handleNoBlocks = pure ()
                        }
                    blockFetchClient
    pure
        (blockFetchClient, mkHeaderIntersector blockIntersector)

nextChainRange
    :: MonadSTM m
    => [a]
    -> STM m (ChainRange a, EventQueueLength)
nextChainRange xs = do
    case xs of
        [] -> retry
        y : ys -> pure
            . (,EventQueueLength (length xs))
            . ChainRange y
            $ case ys of
                [] -> y
                _ -> last ys

queue
    :: Int
    -> IO (a -> IO (), IO (ChainRange a, [a], EventQueueLength), IO ())
queue size = do
    q <- newTBQueueIO $ fromIntegral size
    let push = writeTBQueue q
        flush = do
            xs <- flushTBQueue q
            (range, len) <- nextChainRange xs
            pure (range, xs, len)

        waitEmpty = do
            isEmpty <- isEmptyTBQueue q
            unless isEmpty retry
    pure (atomically <$> push, atomically flush, atomically waitEmpty)
