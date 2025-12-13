module Cardano.N2N.Client.Application.Run
    ( main
    )
where

import CSMT.Backend.RocksDB
    ( RunRocksDB (..)
    , rocksDBBackend
    , withRocksDB
    )
import CSMT.Hashes (delete, insert, mkHash)
import Cardano.N2N.Client.Application.BlockFetch
    ( mkBlockFetchApplication
    )
import Cardano.N2N.Client.Application.ChainSync
    ( mkChainSyncApplication
    )
import Cardano.N2N.Client.Application.Metrics
    ( MetricsEvent (..)
    , metricsTracer
    )
import Cardano.N2N.Client.Application.Options
    ( Options (..)
    , optionsParser
    )
import Cardano.N2N.Client.Application.UTxOs (Change (..), uTxOs)
import Cardano.N2N.Client.Ouroboros.Connection (runNodeApplication)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , modifyTVar
    , newTBQueueIO
    , newTVarIO
    , readTBQueue
    , readTVar
    , writeTBQueue
    )
import Control.Exception (throwIO)
import Control.Monad (forM_, forever, (<=<))
import Control.Tracer (Contravariant (..), traceWith)
import Data.ByteString (toStrict)
import OptEnvConf (runParser)
import Paths_cardano_utxo_csmt (version)
import System.IO
    ( BufferMode (..)
    , hSetBuffering
    , stdout
    )

main :: IO ()
main = do
    options <- runParser version "N2N app example" optionsParser
    e <- application options
    putStrLn $ "Synced " ++ show e ++ " blocks."

-- | Run an cardano-n2n-client application that connects to a node and syncs
-- blocks starting from the given point, up to the given limit.
application
    :: Options
    -- ^ limit of blocks to sync
    -> IO ()
application
    Options
        { networkMagic
        , nodeName
        , portNumber
        , startingPoint
        , dbPath
        } = do
        hSetBuffering stdout NoBuffering
        events <- newTBQueueIO 1000
        doneVar <- newTVarIO False
        tracer <- metricsTracer 10
        countVar <- newTVarIO 0
        ops <- newTBQueueIO 1000
        withRocksDB dbPath $ \(RunRocksDB run) -> do
            let counting = do
                    count <- atomically $ do
                        modifyTVar countVar succ
                        readTVar countVar
                    traceWith tracer (BlockHeightMetrics count)
            link <=< async $ forever $ do
                evt <- atomically $ readTBQueue ops
                case evt of
                    Spend txIn -> do
                        let key = txIn
                        run $ delete (rocksDBBackend mkHash) (toStrict key)
                    Create txIn txOut -> do
                        let key = txIn
                            value' = txOut
                        run $ insert (rocksDBBackend mkHash) (toStrict key) (toStrict value')
            r <-
                runNodeApplication
                    networkMagic
                    nodeName
                    portNumber
                    (mkChainSyncApplication events doneVar startingPoint)
                    $ mkBlockFetchApplication
                        (contramap BlockFetchMetrics tracer)
                        events
                        doneVar
                    $ \block -> do
                        forM_ (uTxOs block) $ \change -> do
                            atomically $ writeTBQueue ops change
                        counting

            case r of
                Left err -> throwIO err
                Right _ -> pure ()
