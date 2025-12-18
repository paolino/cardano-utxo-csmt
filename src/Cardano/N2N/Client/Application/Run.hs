module Cardano.N2N.Client.Application.Run
    ( main
    )
where

import CSMT (Backend)
import CSMT.Backend.RocksDB
    ( RocksDB
    , RunRocksDB (..)
    , rocksDBBackend
    , withRocksDB
    )
import CSMT.Hashes (Hash, delete, insert, mkHash)
import Cardano.N2N.Client.Application.BlockFetch
    ( mkBlockFetchApplication
    )
import Cardano.N2N.Client.Application.ChainSync
    ( Follower (..)
    , ProgressOrRewind (..)
    , mkChainSyncApplication
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
import Cardano.N2N.Client.Ouroboros.Types (Intersector (..))
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , modifyTVar
    , newTVarIO
    , readTVar
    )
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Tracer (Contravariant (..), traceWith)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Lazy (LazyByteString)
import Data.Function (on)
import OptEnvConf (runParser)
import Ouroboros.Consensus.Block (WithOrigin (Origin))
import Ouroboros.Network.Block qualified as Network
import Paths_cardano_utxo_csmt (version)
import System.IO
    ( BufferMode (..)
    , hSetBuffering
    , stdout
    )

main :: IO ()
main = do
    options <- runParser version "Chain following utxos" optionsParser
    e <- application options
    putStrLn $ "Synced " ++ show e ++ " blocks."

rocks :: Backend RocksDB ByteString ByteString Hash
rocks = rocksDBBackend mkHash

deleting :: LazyByteString -> RocksDB ()
deleting = delete rocks . toStrict

inserting :: LazyByteString -> LazyByteString -> RocksDB ()
inserting = insert rocks `on` toStrict

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
        tracer <- metricsTracer 10
        countVar <- newTVarIO 0
        let counting = do
                count <- atomically $ do
                    modifyTVar countVar succ
                    readTVar countVar
                traceWith tracer (BlockHeightMetrics count)
        withRocksDB dbPath $ \(RunRocksDB run) -> do
            let deleteKey key = run $ deleting key
                insertKey key value' = run $ inserting key value'
                blockIntersector =
                    Intersector
                        { intersectFound = \_point -> do
                            pure blockFollower
                        , intersectNotFound = do
                            pure (blockIntersector, [Network.Point Origin])
                        }
                blockFollower =
                    Follower
                        { rollForward = \block -> do
                            forM_ (uTxOs block) $ \change -> do
                                case change of
                                    Spend txIn -> deleteKey txIn
                                    Create txIn txOut -> insertKey txIn txOut
                            counting
                            pure blockFollower
                        , rollBackward = \_point -> do
                            putStrLn "rewind not supported"
                            pure $ Progress blockFollower
                        }
            (blockFetchApplication, headerIntersector) <-
                mkBlockFetchApplication
                    (contramap BlockFetchMetrics tracer)
                    blockIntersector
            let chainFollowingApplication =
                    mkChainSyncApplication
                        headerIntersector
                        startingPoint
            r <-
                runNodeApplication
                    networkMagic
                    nodeName
                    portNumber
                    chainFollowingApplication
                    blockFetchApplication

            case r of
                Left err -> throwIO err
                Right _ -> pure ()
