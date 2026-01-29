{- |
Module      : Mithril.STM.Crypto.Hsblst
Description : BLS12-381 crypto backend using hsblst
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module provides a concrete implementation of the 'BlsOps' function
record using the @hsblst@ library for BLS12-381 elliptic curve operations.

= BLS12-381 Configuration

This uses the G2 scheme in hsblst, which maps to Mithril's \"min-sig\" variant:

* __Signatures__: Points on G1 (48 bytes compressed)
* __Verification keys__: Points on G2 (96 bytes compressed)
* __Pairing__: e(sig, g2) = e(H(msg), pk)

The curve parameter in hsblst's types (e.g., @Signature G2 Hash@) indicates
which curve the __public keys__ use. Signatures automatically use the
complementary curve for the pairing to work.

= Domain Separation

The Mithril protocol uses domain separation tags (DST) for hashing to curve.
The standard DST for Mithril is: @\"MITHRIL-SIGNTRS\"@

= Usage Example

@
import Mithril.STM
import Mithril.STM.Crypto.Hsblst
import Mithril.STM.Crypto.Crypton (cryptonHashOps)

-- Verify a certificate using real crypto
result <- verify cryptonHashOps hsblstBlsOps params avk message aggSig
@
-}
module Mithril.STM.Crypto.Hsblst
    ( -- * Function Records
      hsblstBlsOps

      -- * Concrete Types
    , BlsSignature
    , BlsVerificationKey

      -- * Domain Separation Tag
    , mithrilDst

      -- * Low-level Utilities
    , compressSig
    , decompressSig
    , compressVk
    , decompressVk
    ) where

import Crypto.BLST
    ( BlstError (..)
    , Curve (..)
    , EncodeMethod (..)
    , PublicKey
    , Signature
    , aggregateSignatures
    , aggregateVerify
    , compressPk
    , compressSignature
    , decompressPk
    , decompressSignature
    )
import Data.ByteArray (convert)
import Data.ByteArray.Sized (SizedByteArray, sizedByteArray)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE

import Mithril.STM.Crypto (BlsOps (..))

-- ============================================================================
-- Concrete Types
-- ============================================================================

{- | BLS signature type (G1 point).

In Mithril STM, signatures are points on the G1 curve.
Compressed representation: 48 bytes.

The @G2@ parameter in @Signature G2 Hash@ indicates this signature
is used with G2 public keys. The actual signature point is on G1.
-}
type BlsSignature = Signature G2 Hash

{- | BLS verification key type (G2 point).

In Mithril, verification keys (public keys) are points on the G2 curve.
Compressed representation: 96 bytes.
-}
type BlsVerificationKey = PublicKey G2

-- ============================================================================
-- Domain Separation
-- ============================================================================

{- | Domain separation tag for Mithril signature scheme.

This tag is used when hashing messages to curve points, ensuring
that signatures from different protocols cannot be confused.

From the Mithril Rust implementation:
@DST = \"MITHRIL-SIGNTRS\"@
-}
mithrilDst :: ByteString
mithrilDst = "MITHRIL-SIGNTRS"

-- ============================================================================
-- BlsOps Implementation
-- ============================================================================

{- | BLS operations using hsblst.

Provides aggregate signature verification for Mithril STM certificates.

__Important__: This implementation uses the \"Hash\" encoding method
and the Mithril domain separation tag.
-}
hsblstBlsOps :: BlsOps BlsSignature BlsVerificationKey
hsblstBlsOps =
    BlsOps
        { blsVerifyAggregate = verifyAggregate
        , blsDeserializeSig = decompressSig
        , blsDeserializeVk = decompressVk
        , blsSerializeSig = compressSig
        , blsSerializeVk = compressVk
        }

{- | Verify an aggregate BLS signature.

Takes a message and a list of (verification key, signature) pairs.
Returns True if the aggregate signature is valid.

__Algorithm__:

1. Aggregate all signatures into a single signature
2. Verify that e(aggSig, g2) = prod(e(H(msg), pk_i))

This uses the \"same message\" variant where all signers sign
the same message.
-}
verifyAggregate
    :: ByteString -> [(BlsVerificationKey, BlsSignature)] -> Bool
verifyAggregate _msg [] = False
verifyAggregate msg pairs =
    case aggregateAndVerify msg pairs of
        Right True -> True
        _ -> False

-- | Aggregate signatures and verify.
aggregateAndVerify
    :: ByteString
    -> [(BlsVerificationKey, BlsSignature)]
    -> Either BlstError Bool
aggregateAndVerify msg pairs = do
    let sigs = map snd pairs
        pks = map fst pairs
        pkMsgPairs = map (\pk -> (pk, msg)) pks
    case NE.nonEmpty sigs of
        Nothing -> Left BlstBadEncoding
        Just neSigs -> do
            let aggSig = aggregateSignatures neSigs
            aggregateVerify
                (NE.fromList pkMsgPairs)
                aggSig
                (Just mithrilDst)

-- ============================================================================
-- Serialization Utilities
-- ============================================================================

{- | Compress a BLS signature to 48 bytes (G1 point).

With the G2 scheme, signatures are G1 points (48 bytes compressed),
while public keys are G2 points (96 bytes). This matches Mithril's
min-sig format.
-}
compressSig :: BlsSignature -> ByteString
compressSig sig = convert (compressSignature sig)

-- | Decompress a BLS signature from 48 bytes (G1 point).
decompressSig :: ByteString -> Maybe BlsSignature
decompressSig bs = do
    sized <- sizedByteArray bs :: Maybe (SizedByteArray 48 ByteString)
    case decompressSignature sized of
        Left _ -> Nothing
        Right sig -> Just sig

-- | Compress a BLS verification key to 96 bytes.
compressVk :: BlsVerificationKey -> ByteString
compressVk pk = convert (compressPk pk)

-- | Decompress a BLS verification key from 96 bytes.
decompressVk :: ByteString -> Maybe BlsVerificationKey
decompressVk bs = do
    sized <- sizedByteArray bs :: Maybe (SizedByteArray 96 ByteString)
    case decompressPk sized of
        Left _ -> Nothing
        Right pk -> Just pk
