{- |
Module      : Mithril.STM.Crypto.Crypton
Description : Hash operations using crypton (Blake2b)
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module provides Blake2b hash operations using the @crypton@ library.
Unlike the hsblst module, this uses 'ByteString' as the hash output type,
which makes serialization straightforward.

= Hash Functions

* __Blake2b-256__: 32-byte output, used for Merkle tree hashing
* __Blake2b-512__: 64-byte output, used for lottery hash computation

= Usage Example

@
import Mithril.STM.Crypto.Crypton

-- Use with verification
result <- verify cryptonHashOps blsOps params avk message aggSig
@
-}
module Mithril.STM.Crypto.Crypton
    ( -- * Function Records
      cryptonHashOps

      -- * Direct Hash Functions
    , hashBlake2b256
    , hashBlake2b512
    ) where

import Crypto.Hash (Blake2b_256, Blake2b_512, Digest, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import Mithril.STM.Crypto (HashOps (..))

-- ============================================================================
-- HashOps Implementation
-- ============================================================================

{- | Hash operations using crypton's Blake2b with ByteString output.

This is the recommended HashOps for production use as it:

* Uses crypton's optimized Blake2b implementation
* Returns ByteString for easy serialization
* Validates hash sizes on deserialization
-}
cryptonHashOps :: HashOps ByteString ByteString
cryptonHashOps =
    HashOps
        { blake2b256 = hashBlake2b256
        , blake2b512 = hashBlake2b512
        , hash256ToBytes = id
        , hash512ToBytes = id
        , bytesToHash256 = validateHash256
        , bytesToHash512 = validateHash512
        }

-- ============================================================================
-- Direct Hash Functions
-- ============================================================================

{- | Compute Blake2b-256 hash (32 bytes).

Used for:

* Merkle tree node hashing
* Message transformation in verification
-}
hashBlake2b256 :: ByteString -> ByteString
hashBlake2b256 bs = convert (hash bs :: Digest Blake2b_256)

{- | Compute Blake2b-512 hash (64 bytes).

Used for:

* Lottery eligibility hash computation
* The hash is interpreted as a number in [0, 2^512) for lottery
-}
hashBlake2b512 :: ByteString -> ByteString
hashBlake2b512 bs = convert (hash bs :: Digest Blake2b_512)

-- ============================================================================
-- Validation
-- ============================================================================

-- | Validate a 256-bit hash (32 bytes).
validateHash256 :: ByteString -> Maybe ByteString
validateHash256 bs
    | BS.length bs == 32 = Just bs
    | otherwise = Nothing

-- | Validate a 512-bit hash (64 bytes).
validateHash512 :: ByteString -> Maybe ByteString
validateHash512 bs
    | BS.length bs == 64 = Just bs
    | otherwise = Nothing
