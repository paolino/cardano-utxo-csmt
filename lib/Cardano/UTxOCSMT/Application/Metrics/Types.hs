{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Cardano.UTxOCSMT.Application.Metrics.Types
Description : Data types for metrics collection

This module defines the core data types used for metrics collection:

* 'BootstrapPhase' - phases during application startup
* 'MetricsEvent' - events that update metrics state
* 'ExtractionProgress' - progress of UTxO extraction
* 'HeaderSyncProgress' - progress of header synchronization
* 'Metrics' - the aggregated metrics state
* 'MetricsParams' - configuration for metrics collection
-}
module Cardano.UTxOCSMT.Application.Metrics.Types
    ( -- * Bootstrap phases
      BootstrapPhase (..)

      -- * Metrics events
    , MetricsEvent (..)
    , _BlockFetchEvent
    , _UTxOChangeEvent
    , _BlockInfoEvent
    , _MerkleRootEvent
    , _BaseCheckpointEvent
    , _ChainTipEvent
    , _BootstrapPhaseEvent
    , _ExtractionTotalEvent
    , _ExtractionProgressEvent
    , _HeaderSyncProgressEvent
    , _DownloadProgressEvent

      -- * Progress types
    , ExtractionProgress (..)
    , HeaderSyncProgress (..)

      -- * Metrics state
    , Metrics (..)
    , MetricsParams (..)

      -- * Render utilities
    , renderBlockPoint
    , renderPoint
    )
where

import CSMT.Hashes (Hash)
import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength (..)
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Header, Point)
import Control.Lens
    ( (&)
    , (.~)
    , (?~)
    )
import Control.Lens.TH (makePrisms)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Proxy (Proxy (..))
import Data.Swagger
    ( ToSchema (..)
    , declareSchemaRef
    , description
    , properties
    , required
    )
import Data.Swagger qualified as Swagger
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.IsList (IsList (..))
import Ouroboros.Network.Block (SlotNo (..), blockPoint)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (Block (..), WithOrigin (..))

-- | Bootstrap phase during startup
data BootstrapPhase
    = -- | Downloading Mithril snapshot
      Downloading
    | -- | Counting UTxOs in snapshot
      Counting
    | -- | Extracting UTxOs from Mithril snapshot
      Extracting
    | -- | Syncing headers after Mithril import
      SyncingHeaders
    | -- | Fully synced with chain tip
      Synced
    deriving (Show, Eq)

instance ToJSON BootstrapPhase where
    toJSON = \case
        Downloading -> "downloading"
        Counting -> "counting"
        Extracting -> "extracting"
        SyncingHeaders -> "syncing_headers"
        Synced -> "synced"

instance ToSchema BootstrapPhase where
    declareNamedSchema _ =
        return
            $ Swagger.NamedSchema (Just "BootstrapPhase")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerString
            & Swagger.enum_
                ?~ [ "downloading"
                   , "counting"
                   , "extracting"
                   , "syncing_headers"
                   , "synced"
                   ]
            & description
                ?~ "Current bootstrap phase: extracting, syncing_headers, \
                   \or synced"

-- | The signal we receive to update the metrics
data MetricsEvent
    = -- | some blocks are going to be fetched
      BlockFetchEvent EventQueueLength
    | -- | one utxo change has been processed
      UTxOChangeEvent
    | -- | one block has been processed
      BlockInfoEvent Header
    | -- | the current merkle root
      MerkleRootEvent Hash
    | -- | the base checkpoint for the UTxO set
      BaseCheckpointEvent Point
    | -- | the current chain tip slot from ChainSync
      ChainTipEvent SlotNo
    | -- | current bootstrap phase
      BootstrapPhaseEvent BootstrapPhase
    | -- | extraction total (from decoded ledger state)
      ExtractionTotalEvent Word64
    | -- | extraction progress (UTxOs extracted so far)
      ExtractionProgressEvent Word64
    | -- | header sync progress (current slot, target slot)
      HeaderSyncProgressEvent SlotNo SlotNo
    | -- | download progress (bytes downloaded so far)
      DownloadProgressEvent Word64
    deriving (Show)

makePrisms ''MetricsEvent

-- | Progress of UTxO extraction from Mithril snapshot
data ExtractionProgress = ExtractionProgress
    { extractionCurrent :: Word64
    -- ^ Number of UTxOs extracted so far
    , extractionTotal :: Maybe Word64
    -- ^ Total number of UTxOs to extract (if known)
    , extractionPercent :: Maybe Double
    -- ^ Percentage complete (if total is known)
    , extractionRate :: Double
    -- ^ Extraction rate (UTxOs per second)
    }
    deriving (Show, Eq)

instance ToJSON ExtractionProgress where
    toJSON
        ExtractionProgress
            { extractionCurrent
            , extractionTotal
            , extractionPercent
            , extractionRate
            } =
            object
                [ "current" .= extractionCurrent
                , "total" .= extractionTotal
                , "percent" .= extractionPercent
                , "rate" .= extractionRate
                ]

instance ToSchema ExtractionProgress where
    declareNamedSchema _ = do
        word64Schema <- declareSchemaRef (Proxy @Word64)
        maybeWord64Schema <- declareSchemaRef (Proxy @(Maybe Word64))
        maybeDoubleSchema <- declareSchemaRef (Proxy @(Maybe Double))
        doubleSchema <- declareSchemaRef (Proxy @Double)
        return
            $ Swagger.NamedSchema (Just "ExtractionProgress")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("current", word64Schema)
                    , ("total", maybeWord64Schema)
                    , ("percent", maybeDoubleSchema)
                    , ("rate", doubleSchema)
                    ]
            & required .~ ["current", "rate"]
            & description
                ?~ "Progress of UTxO extraction from Mithril snapshot"

-- | Progress of header synchronization after Mithril import
data HeaderSyncProgress = HeaderSyncProgress
    { headerCurrentSlot :: SlotNo
    -- ^ Current slot being processed
    , headerTargetSlot :: SlotNo
    -- ^ Target slot to reach
    }
    deriving (Show, Eq)

instance ToJSON HeaderSyncProgress where
    toJSON HeaderSyncProgress{headerCurrentSlot, headerTargetSlot} =
        object
            [ "currentSlot" .= unSlotNo headerCurrentSlot
            , "targetSlot" .= unSlotNo headerTargetSlot
            ]

instance ToSchema HeaderSyncProgress where
    declareNamedSchema _ = do
        word64Schema <- declareSchemaRef (Proxy @Word64)
        return
            $ Swagger.NamedSchema (Just "HeaderSyncProgress")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("currentSlot", word64Schema)
                    , ("targetSlot", word64Schema)
                    ]
            & required .~ ["currentSlot", "targetSlot"]
            & description
                ?~ "Progress of header synchronization after Mithril import"

-- | Tracked metrics
data Metrics = Metrics
    { averageQueueLength :: Double
    , maxQueueLength :: Maybe Int
    , utxoChangesCount :: Int
    , lastBlockPoint :: Maybe (UTCTime, Header)
    , utxoSpeed :: Double
    , blockSpeed :: Double
    , currentEra :: Maybe String
    , currentMerkleRoot :: Maybe Hash
    , baseCheckpoint :: Maybe Point
    , chainTipSlot :: Maybe SlotNo
    -- ^ The current chain tip slot from ChainSync protocol
    , bootstrapPhase :: Maybe BootstrapPhase
    -- ^ Current bootstrap phase during startup
    , extractionProgress :: Maybe ExtractionProgress
    -- ^ Progress of UTxO extraction from Mithril snapshot
    , headerSyncProgress :: Maybe HeaderSyncProgress
    -- ^ Progress of header synchronization after Mithril import
    , downloadedBytes :: Maybe Word64
    -- ^ Bytes downloaded during Mithril snapshot download
    }
    deriving (Show)

instance ToJSON Metrics where
    toJSON
        Metrics
            { averageQueueLength
            , maxQueueLength
            , utxoChangesCount
            , lastBlockPoint
            , utxoSpeed
            , blockSpeed
            , currentEra
            , currentMerkleRoot
            , baseCheckpoint
            , chainTipSlot
            , bootstrapPhase
            , extractionProgress
            , headerSyncProgress
            , downloadedBytes
            } =
            object
                [ "averageQueueLength" .= averageQueueLength
                , "maxQueueLength" .= maxQueueLength
                , "utxoChangesCount" .= utxoChangesCount
                , "lastBlockPoint"
                    .= fmap (Text.pack . renderBlockPoint) lastBlockPoint
                , "utxoSpeed" .= utxoSpeed
                , "blockSpeed" .= blockSpeed
                , "currentEra" .= currentEra
                , "currentMerkleRoot" .= fmap (Text.pack . show) currentMerkleRoot
                , "baseCheckpoint" .= fmap (Text.pack . renderPoint) baseCheckpoint
                , "chainTipSlot" .= fmap unSlotNo chainTipSlot
                , "bootstrapPhase" .= bootstrapPhase
                , "extractionProgress" .= extractionProgress
                , "headerSyncProgress" .= headerSyncProgress
                , "downloadedBytes" .= downloadedBytes
                ]

-- | Render a block point as a string for display
renderBlockPoint :: (a, Header) -> [Char]
renderBlockPoint (_, header) = renderPoint $ blockPoint header

-- | Render a Point as a string for display
renderPoint
    :: Point -> [Char]
renderPoint (Network.Point Origin) = "Origin"
renderPoint (Network.Point (At block)) =
    show (blockPointHash block)
        ++ "@"
        ++ show (unSlotNo $ blockPointSlot block)

instance ToSchema Metrics where
    declareNamedSchema _ = do
        doubleSchema <- declareSchemaRef (Proxy @Double)
        maybeIntSchema <- declareSchemaRef (Proxy @(Maybe Int))
        intSchema <- declareSchemaRef (Proxy @Int)
        maybeStringSchema <- declareSchemaRef (Proxy @(Maybe String))
        maybeWord64Schema <- declareSchemaRef (Proxy @(Maybe Word64))
        maybeBootstrapPhaseSchema <-
            declareSchemaRef (Proxy @(Maybe BootstrapPhase))
        maybeExtractionProgressSchema <-
            declareSchemaRef (Proxy @(Maybe ExtractionProgress))
        maybeHeaderSyncProgressSchema <-
            declareSchemaRef (Proxy @(Maybe HeaderSyncProgress))
        return
            $ Swagger.NamedSchema (Just "Metrics")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("averageQueueLength", doubleSchema)
                    , ("maxQueueLength", maybeIntSchema)
                    , ("utxoChangesCount", intSchema)
                    , ("lastBlockPoint", maybeStringSchema)
                    , ("utxoSpeed", doubleSchema)
                    , ("blockSpeed", doubleSchema)
                    , ("currentEra", maybeStringSchema)
                    , ("currentMerkleRoot", maybeStringSchema)
                    , ("baseCheckpoint", maybeStringSchema)
                    , ("chainTipSlot", maybeWord64Schema)
                    , ("bootstrapPhase", maybeBootstrapPhaseSchema)
                    , ("extractionProgress", maybeExtractionProgressSchema)
                    , ("headerSyncProgress", maybeHeaderSyncProgressSchema)
                    , ("downloadedBytes", maybeWord64Schema)
                    ]
            & required
                .~ [ "averageQueueLength"
                   , "maxQueueLength"
                   , "utxoChangesCount"
                   , "lastBlockPoint"
                   , "utxoSpeed"
                   , "blockSpeed"
                   , "currentEra"
                   , "currentMerkleRoot"
                   , "baseCheckpoint"
                   , "chainTipSlot"
                   , "bootstrapPhase"
                   , "extractionProgress"
                   , "headerSyncProgress"
                   , "downloadedBytes"
                   ]
            & description
                ?~ "Metrics about CSMT operations and blockchain synchronization"

-- | Metrics configuration parameters
data MetricsParams = MetricsParams
    { qlWindow :: Int
    -- ^ how many samples to consider for average queue length
    , utxoSpeedWindow :: Int
    -- ^ how many samples to consider for speed calculation
    , blockSpeedWindow :: Int
    -- ^ how many samples to consider for speed calculation
    , metricsOutput :: Metrics -> IO ()
    -- ^ function to output the metrics
    , metricsFrequency :: Int
    -- ^ frequency in microseconds to output the metrics
    }
