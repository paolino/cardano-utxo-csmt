{-# LANGUAGE NumericUnderscores #-}

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
    ( Metrics (..)
    , MetricsEvent (..)
    , MetricsParams (..)
    , metricsTracer
    )
import Cardano.N2N.Client.Application.Options
    ( Options (..)
    , optionsParser
    )
import Cardano.N2N.Client.Application.UTxOs (Change (..), uTxOs)
import Cardano.N2N.Client.Ouroboros.Connection (runNodeApplication)
import Cardano.N2N.Client.Ouroboros.Types (Block, Intersector (..))
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Tracer (Contravariant (..), traceWith)
import Data.ByteString (toStrict)
import Data.Function (fix, on)
import Data.Maybe (fromMaybe)
import OptEnvConf (runParser)
import Ouroboros.Consensus.Block (blockNo, blockPoint, unSlotNo)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point
    ( WithOrigin (..)
    , blockPointHash
    , blockPointSlot
    )
import Paths_cardano_utxo_csmt (version)
import System.Console.ANSI (hClearScreen, hSetCursorPosition)
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

renderMetrics :: Metrics -> IO ()
renderMetrics
    Metrics
        { averageQueueLength
        , maxQueueLength
        , utxoChangesCount
        , lastBlockPoint
        , utxoSpeed
        , blockSpeed
        , currentEra
        } = do
        hClearScreen stdout
        hSetCursorPosition stdout 0 0
        putStrLn
            $ "Average Queue Length: "
                ++ show averageQueueLength
                ++ "\nMax Queue Length: "
                ++ show maxQueueLength
                ++ "\nTotal utxo changes processed: "
                ++ show utxoChangesCount
                ++ "\nUTXO Change Speed (utxo changes/sec): "
                ++ show utxoSpeed
                ++ "\nBlock Processing Speed (blocks/sec): "
                ++ show blockSpeed
                ++ "\nLast Block Point: "
                ++ maybe "N/A" renderBlockPoint lastBlockPoint
                ++ "\nLast Block Number: "
                ++ maybe "N/A" (show . blockNo . snd) lastBlockPoint
                ++ "\nLast Received Block Time: "
                ++ maybe "N/A" (show . fst) lastBlockPoint
                ++ "\nCurrent Era: "
                ++ fromMaybe "N/A" currentEra
      where
        renderBlockPoint (_, header) = case blockPoint header of
            Network.Point Origin -> "Origin"
            Network.Point (At block) ->
                show (blockPointHash block)
                    ++ "@"
                    ++ show (unSlotNo $ blockPointSlot block)

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
        tracer <-
            metricsTracer
                $ MetricsParams
                    { qlWindow = 100
                    , utxoSpeedWindow = 1000
                    , blockSpeedWindow = 100
                    , metricsOutput = renderMetrics
                    , metricsFrequency = 1_000_000
                    }
        let counting = traceWith tracer UTxOChangeEvent
        withRocksDB dbPath 1 1 $ \runDb -> do
            (blockFetchApplication, headerIntersector) <-
                mkBlockFetchApplication
                    headersQueueSize
                    (contramap BlockFetchEvent tracer)
                    $ blockIntersector
                    $ forwarding runDb counting
            let chainFollowingApplication =
                    mkChainSyncApplication
                        (contramap BlockInfoEvent tracer)
                        headerIntersector
                        [startingPoint]
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
            Spend txIn ->
                -- pure ()
                run $ deleting txIn
            Create txIn txOut ->
                -- pure ()
                run $ inserting txIn txOut
        counting
  where
    rocks = rocksDBBackend mkHash
    deleting = delete rocks . toStrict
    inserting = insert rocks `on` toStrict

blockIntersector :: (Block -> IO ()) -> Intersector Block
blockIntersector forward =
    Intersector
        { intersectFound = \_point -> do
            pure (blockFollower forward)
        , intersectNotFound = do
            pure (blockIntersector forward, [Network.Point Origin])
        }

blockFollower :: (Block -> IO ()) -> Follower Block
blockFollower forward = fix $ \go ->
    Follower
        { rollForward = \block -> do
            forward block
            pure go
        , rollBackward = \_point -> do
            pure $ Progress go
        }
