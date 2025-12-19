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
import Cardano.N2N.Client.Ouroboros.Types (Block, Intersector (..))
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , modifyTVar
    , newTVarIO
    , readTVar
    )
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Tracer (Contravariant (..), Tracer, traceWith)
import Data.ByteString (toStrict)
import Data.Function (fix, on)
import Data.Word (Word32)
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
    options <-
        runParser version "Tracking cardano UTxOs in a CSMT" optionsParser
    application options

application
    :: Options
    -> IO ()
application
    Options
        { networkMagic
        , nodeName
        , portNumber
        , startingPoint
        , headersQueueSize
        , dbPath
        } = do
        hSetBuffering stdout NoBuffering
        tracer <- metricsTracer 10
        counting <- newCounter $ contramap BlockHeightMetrics tracer
        withRocksDB dbPath $ \runDb -> do
            (blockFetchApplication, headerIntersector) <-
                mkBlockFetchApplication
                    headersQueueSize
                    (contramap BlockFetchMetrics tracer)
                    $ blockIntersector
                    $ forwarding runDb counting
            let chainFollowingApplication =
                    mkChainSyncApplication headerIntersector [startingPoint]
            result <-
                runNodeApplication
                    networkMagic
                    nodeName
                    portNumber
                    chainFollowingApplication
                    blockFetchApplication

            case result of
                Left err -> throwIO err
                Right _ -> pure ()

forwarding :: RunRocksDB -> IO () -> Block -> IO ()
forwarding (RunRocksDB run) counting block = do
    forM_ (uTxOs block) $ \change -> do
        case change of
            Spend txIn -> run $ deleting txIn
            Create txIn txOut -> run $ inserting txIn txOut
        counting
  where
    rocks = rocksDBBackend mkHash
    deleting = delete rocks . toStrict
    inserting = insert rocks `on` toStrict

newCounter :: Tracer IO Word32 -> IO (IO ())
newCounter tracer = do
    countVar <- newTVarIO 0
    pure $ do
        count <- atomically $ do
            modifyTVar countVar succ
            readTVar countVar
        traceWith tracer count

blockIntersector :: (Block -> IO ()) -> Intersector Block
blockIntersector forward =
    Intersector
        { intersectFound = \_point -> do
            pure (blockFollower forward)
        , intersectNotFound = do
            putStrLn "intersect not found, starting from Origin"
            pure (blockIntersector forward, [Network.Point Origin])
        }

blockFollower :: (Block -> IO ()) -> Follower Block
blockFollower forward = fix $ \go ->
    Follower
        { rollForward = \block -> do
            forward block
            pure go
        , rollBackward = \_point -> do
            putStrLn $ "rolling back to " ++ show _point
            pure $ Progress go
        }
