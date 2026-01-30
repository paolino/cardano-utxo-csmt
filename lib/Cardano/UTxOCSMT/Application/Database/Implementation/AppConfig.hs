{- |
Module      : Cardano.UTxOCSMT.Application.Database.Implementation.AppConfig
Description : Application configuration stored in database
Copyright   : (c) Paolo Veronelli, 2024
License     : Apache-2.0

This module defines the application configuration record that is
persisted to the database using CBOR serialization.
-}
module Cardano.UTxOCSMT.Application.Database.Implementation.AppConfig
    ( AppConfig (..)
    , defaultAppConfig
    , encodeAppConfig
    , decodeAppConfig
    )
where

import Codec.Serialise
    ( deserialiseOrFail
    , serialise
    )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word64)

{- | Application configuration stored in the database

This record holds all persistent configuration state:

  * @configBaseCheckpoint@ - The base checkpoint for chain sync
  * @configBootstrapInProgress@ - Whether bootstrap is incomplete
  * @configSkipSlot@ - Target slot for skip mode after Mithril bootstrap
-}
data AppConfig slot = AppConfig
    { configBaseCheckpoint :: Maybe slot
    -- ^ Base checkpoint for chain sync (Nothing if not set)
    , configBootstrapInProgress :: Bool
    -- ^ True if bootstrap was started but not completed
    , configSkipSlot :: Maybe Word64
    -- ^ Target slot for skip mode (cleared when skip completes)
    }
    deriving stock (Show, Eq)

-- | Default configuration with no checkpoint and no bootstrap in progress
defaultAppConfig :: AppConfig slot
defaultAppConfig =
    AppConfig
        { configBaseCheckpoint = Nothing
        , configBootstrapInProgress = False
        , configSkipSlot = Nothing
        }

-- | Serialize AppConfig to bytes using CBOR
encodeAppConfig
    :: (slot -> ByteString)
    -- ^ Slot encoder
    -> AppConfig slot
    -> ByteString
encodeAppConfig encodeSlot AppConfig{..} =
    LBS.toStrict
        $ serialise
            ( fmap encodeSlot configBaseCheckpoint
            , configBootstrapInProgress
            , configSkipSlot
            )

-- | Deserialize AppConfig from bytes using CBOR
decodeAppConfig
    :: (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> ByteString
    -> Maybe (AppConfig slot)
decodeAppConfig decodeSlot bs = do
    (mSlotBs, bootstrapInProgress, skipSlot) <-
        either (const Nothing) Just
            $ deserialiseOrFail (LBS.fromStrict bs)
    mSlot <- traverse decodeSlot mSlotBs
    pure
        AppConfig
            { configBaseCheckpoint = mSlot
            , configBootstrapInProgress = bootstrapInProgress
            , configSkipSlot = skipSlot
            }
