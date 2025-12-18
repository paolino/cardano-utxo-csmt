module Cardano.N2N.Client.Application.BlockFetch
    ( mkBlockFetchApplication
    , EventQueueLength (..)
    , BlockFetchApplication
    )
where

import Cardano.N2N.Client.Application.ChainSync
    ( Follower (..)
    , FollowerResult (..)
    )
import Cardano.N2N.Client.Ouroboros.Types
    ( Block
    , BlockFetchApplication
    , Header
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , flushTBQueue
    , isEmptyTBQueue
    , newTBQueueIO
    , writeTBQueue
    )
import Control.Monad (unless)
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

mkBlockFetchApplication
    :: Tracer IO EventQueueLength
    -- ^ metrics tracer
    -> Follower Block
    -- ^ callback to process each fetched block
    -> IO (BlockFetchApplication, Follower Header)
mkBlockFetchApplication tr blockFollower = do
    (pushHeader, flushHeaders, waitEmpty) <- queue 1000
    blockFollowerVar <- newIORef blockFollower -- WTF
    let
        headerFollower =
            Follower
                { rollForward = \header -> do
                    pushHeader $ blockPoint header
                    pure headerFollower
                , rollBackward = \point -> do
                    waitEmpty
                    -- we are safe as the only source of headers is rollForward above
                    withIORef blockFollowerVar $ \Follower{rollBackward} -> do
                        fr <- rollBackward point
                        case fr of
                            Progress bf -> pure (bf, Progress headerFollower)
                            Rewind pts bf -> do
                                pure (bf, Rewind pts headerFollower)
                }
        blockFetchClient = BlockFetchClient $ do
            (points, len) <- flushHeaders
            traceWith tr len
            pure
                $ SendMsgRequestRange
                    points
                    BlockFetchResponse
                        { handleStartBatch = pure
                            $ fix
                            $ \fetchOne ->
                                BlockFetchReceiver
                                    { handleBlock = \block -> do
                                        withIORef_ blockFollowerVar
                                            $ flip rollForward block
                                        pure fetchOne
                                    , handleBatchDone = pure ()
                                    }
                        , handleNoBlocks = pure ()
                        }
                    blockFetchClient
    pure
        (blockFetchClient, headerFollower)

withIORef :: IORef a -> (a -> IO (a, b)) -> IO b
withIORef var f = do
    x <- readIORef var
    (x', y) <- f x
    writeIORef var x'
    pure y

withIORef_ :: IORef t -> (t -> IO t) -> IO ()
withIORef_ var f = withIORef var $ \x -> do
    x' <- f x
    pure (x', ())

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
    :: Int -> IO (a -> IO (), IO (ChainRange a, EventQueueLength), IO ())
queue size = do
    q <- newTBQueueIO $ fromIntegral size
    let push = writeTBQueue q
        flush = flushTBQueue q >>= nextChainRange
        waitEmpty = do
            isEmpty <- isEmptyTBQueue q
            unless isEmpty retry
    pure (atomically <$> push, atomically flush, atomically waitEmpty)
