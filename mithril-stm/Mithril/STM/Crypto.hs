{- |
Module      : Mithril.STM.Crypto
Description : Abstract cryptographic operations via function records
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module defines the cryptographic operations required for STM verification
as __function records__ (also called "handles" or "dictionaries"). This is an
alternative to typeclasses that makes dependencies explicit and testing easier.

= Why Function Records Instead of Typeclasses?

Typeclasses are convenient but have drawbacks:

1. __Implicit passing__: It's not obvious which crypto backend is used
2. __Orphan instances__: Using a different backend requires orphans or newtypes
3. __Testing friction__: Mocking requires newtypes or specific test instances
4. __Global coherence__: Can't easily use two backends in the same program

Function records solve these:

1. __Explicit passing__: The crypto backend is a visible parameter
2. __No instances needed__: Just construct a record with the functions
3. __Easy mocking__: Test records can return canned values
4. __Local choice__: Different records can coexist

= Usage Pattern

@
-- Production: use real crypto
let hashOps = cryptonHashOps
    blsOps = hsblstOps
verify hashOps blsOps params avk msg sig

-- Testing: use mock crypto
let mockHash = MockHashOps { blake2b256 = \\_ -> "mockhash", ... }
    mockBls = MockBlsOps { blsVerifyAggregate = \\_ _ -> True, ... }
verify mockHash mockBls params avk msg sig
@

= Required Operations

== Hashing

STM uses Blake2b in two variants:

* __Blake2b-256__ (32 bytes): For Merkle tree nodes
* __Blake2b-512__ (64 bytes): For lottery evaluation

== BLS Signatures

STM uses BLS12-381 signatures with:

* __Verification keys__ on G2 (96 bytes compressed)
* __Signatures__ on G1 (48 bytes compressed)
* __Aggregate verification__: Verify multiple (key, sig) pairs efficiently

= Performance Considerations

The function record approach has negligible overhead:

* GHC often inlines small records
* The crypto operations themselves dominate runtime
* Verification of ~200 signatures takes ~100ms regardless of dispatch method
-}
module Mithril.STM.Crypto
    ( -- * Hash Operations
      -- $hashOps
      HashOps (..)

      -- * BLS Operations
      -- $blsOps
    , BlsOps (..)
    ) where

import Data.ByteString (ByteString)

{- $hashOps
Hash operations using Blake2b. The STM protocol requires two output sizes:
256-bit (32 bytes) for Merkle trees and 512-bit (64 bytes) for lottery.
-}

{- | Record of hashing operations.

This record provides Blake2b hashing with two output sizes.
The type parameters @hash256@ and @hash512@ allow different
representations (e.g., 'ByteString', a newtype, or a custom type).

__Implementations__:

* @crypton@: "Mithril.STM.Crypto.Crypton" (recommended)
* @cryptonite@: Similar API, but unmaintained
* Mock: Return constant values for testing

==== Example Implementation (using crypton)

@
import Crypto.Hash (Blake2b_256, Blake2b_512, hash)
import Data.ByteArray (convert)

cryptonHashOps :: HashOps ByteString ByteString
cryptonHashOps = HashOps
    { blake2b256 = convert . hash \@Blake2b_256
    , blake2b512 = convert . hash \@Blake2b_512
    , hash256ToBytes = id
    , hash512ToBytes = id
    , bytesToHash256 = Just
    , bytesToHash512 = Just
    }
@
-}
data HashOps hash256 hash512 = HashOps
    { blake2b256 :: ByteString -> hash256
    {- ^ Hash input to 32 bytes using Blake2b-256.

    Used for:

    * Merkle tree leaf hashes
    * Merkle tree internal node hashes
    * Message transformation before BLS verification
    -}
    , blake2b512 :: ByteString -> hash512
    {- ^ Hash input to 64 bytes using Blake2b-512.

    Used for:

    * Lottery evaluation (needs 512 bits for uniform distribution)

    __Why 512 bits?__

    The lottery compares @hash / 2^512@ against a probability.
    Using 512 bits ensures the hash output, when interpreted as
    a fraction, has enough precision for accurate comparison.
    -}
    , hash256ToBytes :: hash256 -> ByteString
    {- ^ Convert hash256 to raw bytes (should be 32 bytes).

    Used when:

    * Serializing Merkle proofs
    * Comparing hash values
    * Building composite hashes (e.g., hash(left || right))
    -}
    , hash512ToBytes :: hash512 -> ByteString
    {- ^ Convert hash512 to raw bytes (should be 64 bytes).

    Used when:

    * Evaluating lottery (interpret bytes as big integer)
    -}
    , bytesToHash256 :: ByteString -> Maybe hash256
    {- ^ Parse 32 bytes as hash256. Returns 'Nothing' if wrong length.

    Used when:

    * Deserializing Merkle proofs
    * Parsing aggregate verification keys
    -}
    , bytesToHash512 :: ByteString -> Maybe hash512
    {- ^ Parse 64 bytes as hash512. Returns 'Nothing' if wrong length.

    Used when:

    * Deserializing (if hash512 values are ever serialized directly)
    -}
    }

{- $blsOps
BLS12-381 signature operations. STM uses "basic" BLS signatures with
public keys on G2 and signatures on G1.
-}

{- | Record of BLS12-381 signature operations.

This record provides BLS signature verification and serialization.
The type parameters @sig@ and @vk@ represent the signature and
verification key types from your chosen BLS library.

__Implementations__:

* @hsblst@: "Mithril.STM.Crypto.Hsblst" (recommended, FFI to BLST)
* @cardano-crypto-class@: Uses same BLST backend
* Mock: Return constant values for testing

==== BLS12-381 Curve Details

BLS12-381 is a pairing-friendly elliptic curve with:

* __G1__: 48-byte compressed points (signatures live here)
* __G2__: 96-byte compressed points (public keys live here)
* __Pairing__: @e: G1 × G2 → GT@ enables signature aggregation

==== Signature Aggregation

Multiple BLS signatures over the same message can be aggregated:

@
e(sig1 + sig2 + ... + sigN, G2) = e(H(msg), pk1 + pk2 + ... + pkN)
@

This is more efficient than verifying N signatures individually.

==== Example Implementation (using hsblst)

@
import qualified Crypto.BLST as BLST

hsblstOps :: BlsOps BLST.Signature BLST.PublicKey
hsblstOps = BlsOps
    { blsVerifyAggregate = \\msg pairs -> ...
    , blsDeserializeSig = BLST.deserialize
    , blsDeserializeVk = BLST.deserialize
    , blsSerializeSig = BLST.serialize
    , blsSerializeVk = BLST.serialize
    }
@
-}
data BlsOps sig vk = BlsOps
    { blsVerifyAggregate
        :: ByteString
        -- \^ Message that was signed
        -> [(vk, sig)]
        -- \^ List of (verification key, signature) pairs
        -> Bool
    {- ^ True if aggregate signature is valid
    ^ Verify an aggregate BLS signature.

    This verifies that:

    @
    e(∑ sig_i, G2) = e(H(msg), ∑ pk_i)
    @

    __Important__: All signatures must be over the SAME message.
    STM transforms the message to include the Merkle commitment,
    ensuring all signers sign the same transformed message.

    __Performance__:

    * Single pairing check regardless of number of signatures
    * Main cost is point additions (~100 signatures in ~50ms)
    * Much faster than N individual verifications
    -}
    , blsDeserializeSig :: ByteString -> Maybe sig
    {- ^ Deserialize a BLS signature from 48 bytes (compressed G1 point).

    Returns 'Nothing' if:

    * Input is not exactly 48 bytes
    * Bytes don't represent a valid G1 point
    * Point is not in the correct subgroup

    __Security__: Subgroup checking is essential to prevent
    small-subgroup attacks.
    -}
    , blsDeserializeVk :: ByteString -> Maybe vk
    {- ^ Deserialize a BLS verification key from 96 bytes (compressed G2 point).

    Returns 'Nothing' if:

    * Input is not exactly 96 bytes
    * Bytes don't represent a valid G2 point
    * Point is not in the correct subgroup
    -}
    , blsSerializeSig :: sig -> ByteString
    {- ^ Serialize a BLS signature to 48 bytes (compressed G1 point).

    The output is the standard compressed format:

    * Bit 7 of byte 0: compression flag (always 1)
    * Bit 6 of byte 0: infinity flag
    * Bit 5 of byte 0: sign of y-coordinate
    * Remaining bits: x-coordinate
    -}
    , blsSerializeVk :: vk -> ByteString
    {- ^ Serialize a BLS verification key to 96 bytes (compressed G2 point).

    Similar compression scheme to signatures but for G2 points.
    -}
    }
