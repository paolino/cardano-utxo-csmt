module Cardano.UTxOCSMT.Application.Run.RenderMetrics
    ( renderMetrics
    )
where

import CSMT ()
import Cardano.UTxOCSMT.Application.Metrics
    ( Metrics (..)
    )
import Data.Maybe (fromMaybe)
import Ouroboros.Network.Block (SlotNo (..), blockNo, blockPoint)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point
    ( WithOrigin (..)
    , blockPointHash
    , blockPointSlot
    )
import System.Console.ANSI (hClearScreen, hSetCursorPosition)
import System.IO (stdout)

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
        , currentMerkleRoot
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
                ++ "\nCurrent Merkle Root: "
                ++ maybe "N/A" show currentMerkleRoot
                ++ "\nCurrent Era: "
                ++ fromMaybe "N/A" currentEra
      where
        renderBlockPoint (_, header) = case blockPoint header of
            Network.Point Origin -> "Origin"
            Network.Point (At block) ->
                show (blockPointHash block)
                    ++ "@"
                    ++ show (unSlotNo $ blockPointSlot block)
