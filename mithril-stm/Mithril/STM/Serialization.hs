{- |
Module      : Mithril.STM.Serialization
Description : Binary serialization for STM types
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module provides binary serialization and deserialization for all
Mithril STM types using the @cereal@ library. The format is compatible
with the Rust @mithril-stm@ crate, enabling interoperability between
Haskell and Rust implementations.

= Serialization Format

All multi-byte integers use __big-endian__ byte order, matching the Rust
implementation. The format follows these conventions:

* 'Word64' values: 8 bytes, big-endian
* 'Word32' values: 4 bytes, big-endian
* 'Double' values: 8 bytes, IEEE 754 big-endian
* Variable-length sequences: 8-byte length prefix (Word64) + elements
* Tagged unions: 1-byte tag + variant data

= Crypto Abstraction

Serialization of cryptographic types (signatures, verification keys, hashes)
is handled through the 'HashOps' and 'BlsOps' function records. This allows
the serialization code to work with any concrete crypto backend.

= Usage Example

@
import Mithril.STM.Serialization
import Data.Serialize (runGet, runPut)

-- Serialize parameters
let bytes = runPut $ putParameters params

-- Deserialize with crypto ops
case runGet (getAggregateSignature sizes hashOps blsOps) bytes of
    Left err -> handleError err
    Right aggSig -> useSignature aggSig
@
-}
module Mithril.STM.Serialization
    ( -- * Size Configuration
      CryptoSizes (..)
    , blsSizes

      -- * Parameters
    , putParameters
    , getParameters

      -- * Registration
    , putRegistrationEntry
    , getRegistrationEntry

      -- * Signatures
    , putSingleSignature
    , getSingleSignature
    , putSignedRegistration
    , getSignedRegistration

      -- * Aggregate Structures
    , putConcatenationProof
    , getConcatenationProof
    , putAggregateSignature
    , getAggregateSignature

      -- * Verification Key
    , putAggregateVerificationKey
    , getAggregateVerificationKey

      -- * Merkle Types
    , putMerkleCommitment
    , getMerkleCommitment
    , putMerkleBatchPath
    , getMerkleBatchPath

      -- * Convenience Functions
    , serialize
    , deserialize
    ) where

import Data.ByteString (ByteString)
import Data.Serialize
    ( Get
    , Put
    , getByteString
    , getWord32be
    , getWord64be
    , getWord8
    , putByteString
    , putWord32be
    , putWord64be
    , putWord8
    , runGet
    , runPut
    )
import Data.Serialize.IEEE754 (getFloat64be, putFloat64be)
import Data.Word (Word8)

import Mithril.STM.Crypto (BlsOps (..), HashOps (..))
import Mithril.STM.Parameters (Parameters (..))
import Mithril.STM.Types
    ( AggregateSignature (..)
    , AggregateVerificationKey (..)
    , ConcatenationProof (..)
    , MerkleBatchPath (..)
    , MerkleCommitment (..)
    , RegistrationEntry (..)
    , SignedRegistration (..)
    , SingleSignature (..)
    )

-- ============================================================================
-- Size Configuration
-- ============================================================================

{- | Sizes of cryptographic values in bytes.

These sizes are needed for deserialization since the binary format
does not include length prefixes for fixed-size crypto values.
-}
data CryptoSizes = CryptoSizes
    { sigSize :: !Int
    -- ^ BLS signature size (typically 48 bytes for G1)
    , vkSize :: !Int
    -- ^ BLS verification key size (typically 96 bytes for G2)
    , hash256Size :: !Int
    -- ^ Blake2b-256 hash size (32 bytes)
    }
    deriving stock (Show, Eq)

{- | Standard BLS12-381 sizes.

* Signature (G1 point, compressed): 48 bytes
* Verification key (G2 point, compressed): 96 bytes
* Hash (Blake2b-256): 32 bytes
-}
blsSizes :: CryptoSizes
blsSizes =
    CryptoSizes
        { sigSize = 48
        , vkSize = 96
        , hash256Size = 32
        }

-- ============================================================================
-- List Serialization Helpers
-- ============================================================================

-- | Serialize a list with Word64 length prefix.
putListWith :: (a -> Put) -> [a] -> Put
putListWith putElem xs = do
    putWord64be (fromIntegral $ length xs)
    mapM_ putElem xs

-- | Deserialize a list with Word64 length prefix.
getListWith :: Get a -> Get [a]
getListWith getElem = do
    len <- getWord64be
    sequence $ replicate (fromIntegral len) getElem

-- ============================================================================
-- Parameters
-- ============================================================================

{- | Serialize protocol parameters.

Format (24 bytes total):

* m: 8 bytes (Word64, big-endian)
* k: 8 bytes (Word64, big-endian)
* phi_f: 8 bytes (Double, IEEE 754 big-endian)
-}
putParameters :: Parameters -> Put
putParameters Parameters{..} = do
    putWord64be paramM
    putWord64be paramK
    putFloat64be paramPhiF

-- | Deserialize protocol parameters.
getParameters :: Get Parameters
getParameters = do
    m <- getWord64be
    k <- getWord64be
    phiF <- getFloat64be
    pure
        Parameters
            { paramM = m
            , paramK = k
            , paramPhiF = phiF
            }

-- ============================================================================
-- Registration Entry
-- ============================================================================

{- | Serialize a registration entry.

Format:

* Verification key: vkSize bytes (via BlsOps)
* Stake: 8 bytes (Word64, big-endian)
-}
putRegistrationEntry :: BlsOps sig vk -> RegistrationEntry vk -> Put
putRegistrationEntry BlsOps{..} RegistrationEntry{..} = do
    putByteString $ blsSerializeVk reVerificationKey
    putWord64be reStake

-- | Deserialize a registration entry.
getRegistrationEntry
    :: CryptoSizes -> BlsOps sig vk -> Get (RegistrationEntry vk)
getRegistrationEntry CryptoSizes{..} BlsOps{..} = do
    vkBytes <- getByteString vkSize
    case blsDeserializeVk vkBytes of
        Nothing -> fail "Failed to deserialize verification key"
        Just vk -> do
            stake <- getWord64be
            pure $ RegistrationEntry vk stake

-- ============================================================================
-- Single Signature
-- ============================================================================

{- | Serialize a single signature.

Format:

* Signature: sigSize bytes (via BlsOps)
* Indices: length-prefixed list of Word64s
-}
putSingleSignature :: BlsOps sig vk -> SingleSignature sig -> Put
putSingleSignature BlsOps{..} SingleSignature{..} = do
    putByteString $ blsSerializeSig ssSignature
    putListWith putWord64be ssIndices

-- | Deserialize a single signature.
getSingleSignature
    :: CryptoSizes -> BlsOps sig vk -> Get (SingleSignature sig)
getSingleSignature CryptoSizes{..} BlsOps{..} = do
    sigBytes <- getByteString sigSize
    case blsDeserializeSig sigBytes of
        Nothing -> fail "Failed to deserialize signature"
        Just sig -> do
            indices <- getListWith getWord64be
            pure $ SingleSignature sig indices

-- ============================================================================
-- Signed Registration
-- ============================================================================

{- | Serialize a signed registration.

Format:

* Single signature (variable)
* Registration entry (variable)
-}
putSignedRegistration
    :: BlsOps sig vk -> SignedRegistration sig vk -> Put
putSignedRegistration blsOps SignedRegistration{..} = do
    putSingleSignature blsOps srSignature
    putRegistrationEntry blsOps srRegistration

-- | Deserialize a signed registration.
getSignedRegistration
    :: CryptoSizes
    -> BlsOps sig vk
    -> Get (SignedRegistration sig vk)
getSignedRegistration sizes blsOps = do
    sig <- getSingleSignature sizes blsOps
    reg <- getRegistrationEntry sizes blsOps
    pure $ SignedRegistration sig reg

-- ============================================================================
-- Merkle Types
-- ============================================================================

{- | Serialize a Merkle commitment.

Format:

* Root hash: hash256Size bytes (via HashOps)
* Number of leaves: 4 bytes (Word32, big-endian)
-}
putMerkleCommitment
    :: HashOps hash256 hash512 -> MerkleCommitment hash256 -> Put
putMerkleCommitment HashOps{..} MerkleCommitment{..} = do
    putByteString $ hash256ToBytes mcRoot
    putWord32be mcNrLeaves

-- | Deserialize a Merkle commitment.
getMerkleCommitment
    :: CryptoSizes
    -> HashOps hash256 hash512
    -> Get (MerkleCommitment hash256)
getMerkleCommitment CryptoSizes{..} HashOps{..} = do
    hashBytes <- getByteString hash256Size
    case bytesToHash256 hashBytes of
        Nothing -> fail "Failed to deserialize hash256"
        Just root -> do
            nrLeaves <- getWord32be
            pure $ MerkleCommitment root nrLeaves

{- | Serialize a Merkle batch path.

Format:

* Siblings: length-prefixed list of hashes
* Indices: length-prefixed list of Word64s
-}
putMerkleBatchPath
    :: HashOps hash256 hash512 -> MerkleBatchPath hash256 -> Put
putMerkleBatchPath HashOps{..} MerkleBatchPath{..} = do
    putListWith (putByteString . hash256ToBytes) mbpSiblings
    putListWith putWord64be mbpIndices

-- | Deserialize a Merkle batch path.
getMerkleBatchPath
    :: CryptoSizes
    -> HashOps hash256 hash512
    -> Get (MerkleBatchPath hash256)
getMerkleBatchPath CryptoSizes{..} HashOps{..} = do
    siblings <- getListWith $ do
        hashBytes <- getByteString hash256Size
        case bytesToHash256 hashBytes of
            Nothing -> fail "Failed to deserialize hash256 in batch path"
            Just h -> pure h
    indices <- getListWith getWord64be
    pure $ MerkleBatchPath siblings indices

-- ============================================================================
-- Concatenation Proof
-- ============================================================================

{- | Serialize a concatenation proof.

Format:

* Signatures: length-prefixed list of SignedRegistrations
* Batch path: MerkleBatchPath
-}
putConcatenationProof
    :: HashOps hash256 hash512
    -> BlsOps sig vk
    -> ConcatenationProof sig vk hash256
    -> Put
putConcatenationProof hashOps blsOps ConcatenationProof{..} = do
    putListWith (putSignedRegistration blsOps) cpSignatures
    putMerkleBatchPath hashOps cpBatchPath

-- | Deserialize a concatenation proof.
getConcatenationProof
    :: CryptoSizes
    -> HashOps hash256 hash512
    -> BlsOps sig vk
    -> Get (ConcatenationProof sig vk hash256)
getConcatenationProof sizes hashOps blsOps = do
    sigs <- getListWith $ getSignedRegistration sizes blsOps
    path <- getMerkleBatchPath sizes hashOps
    pure $ ConcatenationProof sigs path

-- ============================================================================
-- Aggregate Signature
-- ============================================================================

-- | Tag byte for Concatenation variant.
tagConcatenation :: Word8
tagConcatenation = 0x00

{- | Serialize an aggregate signature.

Format:

* Tag: 1 byte (0x00 for Concatenation)
* Data: variant-specific
-}
putAggregateSignature
    :: HashOps hash256 hash512
    -> BlsOps sig vk
    -> AggregateSignature sig vk hash256
    -> Put
putAggregateSignature hashOps blsOps = \case
    Concatenation proof -> do
        putWord8 tagConcatenation
        putConcatenationProof hashOps blsOps proof

-- | Deserialize an aggregate signature.
getAggregateSignature
    :: CryptoSizes
    -> HashOps hash256 hash512
    -> BlsOps sig vk
    -> Get (AggregateSignature sig vk hash256)
getAggregateSignature sizes hashOps blsOps = do
    tag <- getWord8
    case tag of
        t
            | t == tagConcatenation -> do
                proof <- getConcatenationProof sizes hashOps blsOps
                pure $ Concatenation proof
        _ -> fail $ "Invalid AggregateSignature tag: " ++ show tag

-- ============================================================================
-- Aggregate Verification Key
-- ============================================================================

{- | Serialize an aggregate verification key.

Format:

* Merkle commitment (variable)
* Total stake: 8 bytes (Word64, big-endian)
-}
putAggregateVerificationKey
    :: HashOps hash256 hash512
    -> AggregateVerificationKey hash256
    -> Put
putAggregateVerificationKey hashOps AggregateVerificationKey{..} = do
    putMerkleCommitment hashOps avkMerkleCommitment
    putWord64be avkTotalStake

-- | Deserialize an aggregate verification key.
getAggregateVerificationKey
    :: CryptoSizes
    -> HashOps hash256 hash512
    -> Get (AggregateVerificationKey hash256)
getAggregateVerificationKey sizes hashOps = do
    commitment <- getMerkleCommitment sizes hashOps
    totalStake <- getWord64be
    pure $ AggregateVerificationKey commitment totalStake

-- ============================================================================
-- Convenience Functions
-- ============================================================================

-- | Serialize a value to ByteString.
serialize :: Put -> ByteString
serialize = runPut

-- | Deserialize a value from ByteString.
deserialize :: Get a -> ByteString -> Either String a
deserialize = runGet
