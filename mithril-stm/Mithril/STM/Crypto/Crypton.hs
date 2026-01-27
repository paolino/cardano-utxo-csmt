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

      -- * Ed25519 Verification
    , verifyEd25519
    ) where

import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Blake2b_256, Blake2b_512, Digest, hash)
import Crypto.PubKey.Ed25519 qualified as Ed25519
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

-- ============================================================================
-- Ed25519 Verification (for Genesis Certificate)
-- ============================================================================

{- | Verify an Ed25519 signature.

Used for verifying the genesis certificate, which is signed with IOG's
Ed25519 genesis signing key rather than STM signatures.

__Parameters__:

* @publicKey@: 32-byte Ed25519 public key
* @message@: The message that was signed
* @signature@: 64-byte Ed25519 signature

__Returns__:

* @Just True@: Signature is valid
* @Just False@: Signature is invalid
* @Nothing@: Invalid key or signature format

==== Example

@
let genesisVk = "..." -- 32 bytes from genesis.vkey
    message = signedMessageFromCert
    sig = "..." -- 64-byte signature from genesis cert

case verifyEd25519 genesisVk message sig of
    Just True -> putStrLn "Genesis certificate verified!"
    Just False -> putStrLn "Invalid genesis signature"
    Nothing -> putStrLn "Malformed key or signature"
@
-}
verifyEd25519
    :: ByteString
    -- ^ Public key (32 bytes)
    -> ByteString
    -- ^ Message
    -> ByteString
    -- ^ Signature (64 bytes)
    -> Maybe Bool
verifyEd25519 pubKeyBytes message sigBytes = do
    -- Parse the public key (32 bytes)
    pubKey <- case Ed25519.publicKey pubKeyBytes of
        CryptoPassed pk -> Just pk
        CryptoFailed _ -> Nothing

    -- Parse the signature (64 bytes)
    sig <- case Ed25519.signature sigBytes of
        CryptoPassed s -> Just s
        CryptoFailed _ -> Nothing

    -- Verify
    pure $ Ed25519.verify pubKey message sig
