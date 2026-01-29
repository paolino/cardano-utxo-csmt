{-# LANGUAGE NumericUnderscores #-}

module Cardano.UTxOCSMT.Application.Run.RenderMetrics
    ( renderMetrics
    )
where

import CSMT ()
import Cardano.UTxOCSMT.Application.Metrics
    ( ExtractionProgress (..)
    , Metrics (..)
    , renderBlockPoint
    )
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Ouroboros.Network.Block (blockNo)
import System.Console.ANSI (hClearScreen, hSetCursorPosition)
import System.IO (stdout)
import Text.Printf (printf)

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
        , bootstrapPhase
        , extractionProgress
        , downloadedBytes
        } = do
        hClearScreen stdout
        hSetCursorPosition stdout 0 0
        putStrLn
            $ "Bootstrap Phase: "
                ++ maybe "N/A" show bootstrapPhase
                ++ "\nDownload Progress: "
                ++ renderDownloadProgress downloadedBytes
                ++ "\nExtraction Progress: "
                ++ renderExtractionProgress extractionProgress
                ++ "\nAverage Queue Length: "
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

renderDownloadProgress :: Maybe Word64 -> String
renderDownloadProgress Nothing = "N/A"
renderDownloadProgress (Just bytes) =
    show (bytes `div` 1_000_000) ++ " MB"

renderExtractionProgress :: Maybe ExtractionProgress -> String
renderExtractionProgress Nothing = "N/A"
renderExtractionProgress (Just ExtractionProgress{..}) =
    show extractionCurrent
        ++ " / "
        ++ maybe "?" show extractionTotal
        ++ " ("
        ++ maybe "?" (printf "%.1f") extractionPercent
        ++ "%)"
        ++ " @ "
        ++ printf "%.0f" extractionRate
        ++ " UTxO/s"
