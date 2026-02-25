-- |
-- Module      : Cardano.UTxOCSMT.Application.Run.Config
-- Description : Re-exports from config sublibrary
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Thin re-export module. All definitions have moved
-- to "Cardano.UTxOCSMT.Config" in the @config@
-- sublibrary.
module Cardano.UTxOCSMT.Application.Run.Config
    ( withRocksDB
    , config
    , dbConfig
    , utxoColumnFamilies
    , armageddonParams
    , context
    , prisms
    , slotHash
    , mFinality
    , distance
    , encodePoint
    , decodePoint
    )
where

import Cardano.UTxOCSMT.Config
import Database.RocksDB (Config)

-- | Backwards-compatible alias for 'dbConfig'.
config :: Config
config = dbConfig
