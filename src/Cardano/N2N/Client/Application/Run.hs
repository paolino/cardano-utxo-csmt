{-# LANGUAGE NumericUnderscores #-}

module Cardano.N2N.Client.Application.Run
    ( main
    )
where

import CSMT
import CSMT.Backend.RocksDB
    ( RunRocksDB (..)
    , standaloneRocksDBDatabase
    , withRocksDB
    )
import CSMT.Hashes
    ( Hash
    , isoHash
    )
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
import Cardano.N2N.Client.Application.UTxOs (uTxOs)
import Cardano.N2N.Client.Ouroboros.Connection (runNodeApplication)
import Cardano.N2N.Client.Ouroboros.Types
    ( Block
    , Intersector (..)
    , Point
    )
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Tracer (Contravariant (..), traceWith)
import Data.ByteString (ByteString)
import Data.Function (fix)
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

codecs :: StandaloneCodecs ByteString ByteString Hash
codecs =
    StandaloneCodecs
        { keyCodec = id
        , valueCodec = id
        , nodeCodec = isoHash
        }

forwarding :: RunRocksDB -> IO () -> (Point, Block) -> IO ()
forwarding (RunRocksDB run) counting (_point, block) = run $ do
    database <- standaloneRocksDBDatabase codecs
    forM_ (uTxOs block) $ \change -> do
        -- Transaction.run database $ case change of
        --     Spend txIn ->
        --         delete fromKVHashes StandaloneKVCol StandaloneCSMTCol
        --             $ toStrict txIn
        --     Create txIn txOut ->
        --         insert
        --             fromKVHashes
        --             StandaloneKVCol
        --             StandaloneCSMTCol
        --             (toStrict txIn)
        --             (toStrict txOut)
        liftIO counting

blockIntersector
    :: ((Point, Block) -> IO ()) -> Intersector (Point, Block)
blockIntersector forward =
    Intersector
        { intersectFound = \_point -> do
            pure (blockFollower forward)
        , intersectNotFound = do
            pure (blockIntersector forward, [Network.Point Origin])
        }

blockFollower :: ((Point, Block) -> IO ()) -> Follower (Point, Block)
blockFollower forward = fix $ \go ->
    Follower
        { rollForward = \block -> do
            forward block
            pure go
        , rollBackward = \_point -> do
            pure $ Progress go
        }
