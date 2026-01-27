{-# LANGUAGE DeriveFunctor #-}

{- |
Module      : Mithril.STM.Types
Description : Core data types for STM signatures and verification
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module defines the core data types used in STM signature verification.
All types are parameterized over the concrete cryptographic types (signatures,
verification keys, hashes) to allow different backend implementations.

= Type Parameters Convention

Throughout this module, type parameters follow this convention:

* @sig@ - BLS signature type (typically a G1 curve point, 48 bytes compressed)
* @vk@ - BLS verification key type (typically a G2 curve point, 96 bytes compressed)
* @hash@ - Hash output type (typically 32 bytes for Blake2b-256)

= Deriving Functor

Most types derive 'Functor' to allow mapping over the type parameter.
This is useful for:

* Converting between different hash representations
* Deserializing (parse to 'ByteString', then map to concrete type)
* Testing (map to simpler types for property tests)
-}
module Mithril.STM.Types
    ( -- * Basic Type Aliases
      -- $aliases
      Stake
    , LotteryIndex

      -- * Signer Registration
      -- $registration
    , RegistrationEntry (..)

      -- * Individual Signatures
      -- $signatures
    , SingleSignature (..)
    , SignedRegistration (..)

      -- * Aggregate Signatures
      -- $aggregate
    , ConcatenationProof (..)
    , AggregateSignature (..)

      -- * Verification Key
      -- $verificationKey
    , AggregateVerificationKey (..)

      -- * Merkle Tree Types
      -- $merkle
    , MerkleCommitment (..)
    , MerkleBatchPath (..)
    ) where

import Data.Word (Word32, Word64)

{- $aliases
Simple type aliases for clarity and documentation.
-}

{- | Stake amount in Lovelace.

In Cardano, stake is measured in Lovelace (1 ADA = 1,000,000 Lovelace).
The stake determines a signer's probability of winning the lottery.

__Range__: 0 to ~45 billion ADA (45 × 10^15 Lovelace fits in Word64)
-}
type Stake = Word64

{- | Index into the lottery space.

The lottery space has @m@ possible indices (0 to m-1). Each signer can
potentially win multiple indices based on their stake. An index is "won"
if the deterministic lottery function returns true for that index.

__Range__: 0 to @m - 1@ where @m@ is the protocol parameter (e.g., 20973)
-}
type LotteryIndex = Word64

{- $registration
Each signer must register their verification key and stake amount.
The registration is committed to a Merkle tree, allowing efficient
proof that a signer is part of the valid signer set.
-}

{- | A signer's registration entry containing their verification key and stake.

This is the data committed to the Merkle tree of registered signers.
During verification, we check that each signature's claimed registration
is actually in the committed Merkle tree.

__Serialization__: 104 bytes total

* Verification key: 96 bytes (compressed BLS12-381 G2 point)
* Stake: 8 bytes (big-endian Word64)

==== Example

@
entry = RegistrationEntry
    { reVerificationKey = someBlsPublicKey
    , reStake = 1_000_000_000_000  -- 1 million ADA in Lovelace
    }
@
-}
data RegistrationEntry vk = RegistrationEntry
    { reVerificationKey :: !vk
    {- ^ The signer's BLS verification key (public key).
    This is a point on the G2 curve of BLS12-381.
    -}
    , reStake :: !Stake
    {- ^ The signer's stake in Lovelace.
    Determines lottery win probability.
    -}
    }
    deriving stock (Show, Eq, Functor)

{- $signatures
Individual signatures from single signers, before aggregation.
-}

{- | A single signer's signature with their winning lottery indices.

When a signer participates in STM signing:

1. They check which lottery indices they won (based on stake and message)
2. They create a BLS signature over the message
3. They include the list of indices they're claiming

During verification, we check:

* Each claimed index is actually won (lottery check)
* The BLS signature is valid

__Serialization__:

* Signature: 48 bytes (compressed BLS12-381 G1 point)
* Indices: variable length (count + indices as Word64s)
-}
data SingleSignature sig = SingleSignature
    { ssSignature :: !sig
    {- ^ The BLS signature over the message.
    This is a point on the G1 curve of BLS12-381.
    -}
    , ssIndices :: ![LotteryIndex]
    {- ^ The lottery indices this signer claims to have won.
    Each index must pass the lottery check during verification.
    Typical range: 1-10 indices per signer depending on stake.
    -}
    }
    deriving stock (Show, Eq, Functor)

{- | A signature bundled with the signer's registration information.

This pairs a 'SingleSignature' with the 'RegistrationEntry' of the signer.
The registration info is needed to:

* Look up the signer's stake (for lottery verification)
* Get the verification key (for BLS signature verification)
* Verify Merkle membership (prove signer is registered)
-}
data SignedRegistration sig vk = SignedRegistration
    { srSignature :: !(SingleSignature sig)
    -- ^ The signer's signature and claimed lottery indices.
    , srRegistration :: !(RegistrationEntry vk)
    {- ^ The signer's registration (verification key + stake).
    Must be proven to exist in the Merkle commitment.
    -}
    }
    deriving stock (Show, Eq)

{- $aggregate
Aggregate signatures combine multiple individual signatures into
a single verifiable proof.
-}

{- | Concatenation proof: the simplest form of STM aggregate signature.

In the "concatenation" variant of STM, the aggregate signature is simply
the concatenation of all individual signatures plus a batch Merkle proof
that all signers are registered.

__Why "concatenation"?__

This is called concatenation because we literally concatenate all the
signatures together, as opposed to a future "SNARK" variant that would
use zero-knowledge proofs to compress the signatures.

__Verification steps__:

1. For each signature, verify lottery wins
2. Collect all unique winning indices, check count >= k
3. Verify the batch Merkle proof covers all signers
4. Aggregate and verify all BLS signatures

__Size consideration__:

A concatenation proof with N signers is roughly:
@N × (48 + 8×indices + 104) + Merkle proof size@ bytes

For mainnet with ~200 signers, this is typically 30-50 KB.
-}
data ConcatenationProof sig vk hash = ConcatenationProof
    { cpSignatures :: ![SignedRegistration sig vk]
    {- ^ All individual signatures with their signer info.
    Typically 100-300 signatures on mainnet.
    -}
    , cpBatchPath :: !(MerkleBatchPath hash)
    {- ^ Merkle batch proof that all signers are in the registration tree.
    A batch proof is more efficient than individual proofs.
    -}
    }
    deriving stock (Show, Eq)

{- | Aggregate STM signature.

Currently only the 'Concatenation' variant is implemented.
The Mithril protocol reserves space for future variants:

* __Concatenation__ (0x00): Current implementation, explicit signature list
* __Future/SNARK__ (0x01): Placeholder for ZK-proof compressed signatures

__Serialization__:

* 1 byte: variant tag (0x00 for Concatenation)
* Remaining: variant-specific data
-}
data AggregateSignature sig vk hash
    = -- | The concatenation variant: all signatures explicitly listed.
      Concatenation !(ConcatenationProof sig vk hash)
    deriving stock (Show, Eq)

{- $verificationKey
The aggregate verification key commits to the set of registered signers.
-}

{- | Aggregate verification key for STM signature verification.

This is NOT a single BLS public key. Instead, it contains:

* A Merkle tree commitment to all registered signers
* The total stake of all registered signers

The Merkle commitment allows verifying that claimed signers are actually
part of the registered set without having the full list.

__Derivation__:

The aggregate verification key is derived from the closed registration:

1. Collect all (verification key, stake) pairs from registered signers
2. Build a Merkle tree over the registration entries
3. Sum all stakes to get total stake
4. The root + total stake form the aggregate verification key
-}
data AggregateVerificationKey hash = AggregateVerificationKey
    { avkMerkleCommitment :: !(MerkleCommitment hash)
    {- ^ Commitment to the Merkle tree of registered signers.
    The root hash commits to all (vk, stake) pairs.
    -}
    , avkTotalStake :: !Stake
    {- ^ Sum of all registered signers' stakes.
    Used as denominator in lottery probability calculation.
    -}
    }
    deriving stock (Show, Eq, Functor)

{- $merkle
Merkle tree types for committing to and proving signer registration.
-}

{- | Commitment to a Merkle tree of registered signers.

The Merkle tree is built over 'RegistrationEntry' values (serialized).
The tree uses Blake2b-256 as the hash function.

__Tree structure__:

* Leaves: Hash of serialized RegistrationEntry
* Internal nodes: Hash(left child || right child)
* Root: The final hash committing to all leaves

__Why include height?__

The height is needed for verification to know when we've reached the root.
It also provides a sanity check that the proof has the right structure.
-}
data MerkleCommitment hash = MerkleCommitment
    { mcRoot :: !hash
    {- ^ The Merkle tree root hash (32 bytes for Blake2b-256).
    Commits to all registered signers.
    -}
    , mcNrLeaves :: !Word32
    {- ^ Number of leaves in the tree (number of registered signers).
    Used to derive tree height: @ceiling(log2(nrLeaves))@
    -}
    }
    deriving stock (Show, Eq, Functor)

{- | Batch Merkle path proving multiple leaves belong to the tree.

A batch path is more efficient than individual paths when proving
multiple leaves. Instead of O(N × log M) hashes for N leaves in a
tree of M total leaves, a batch path requires only O(N + log M) hashes.

__Structure__:

The batch path contains:

* Sibling hashes needed to reconstruct the root
* Indices indicating the position of each proven leaf

__Verification__:

To verify a batch path:

1. Hash each leaf (RegistrationEntry)
2. Use the path to reconstruct intermediate nodes
3. Check that the final computed root matches the commitment
-}
data MerkleBatchPath hash = MerkleBatchPath
    { mbpSiblings :: ![hash]
    {- ^ Sibling hashes needed to compute the root.
    These are the "other" children at each level of the tree.
    -}
    , mbpIndices :: ![Word64]
    {- ^ Leaf indices being proven (positions in the tree).
    Determines which side (left/right) at each level.
    -}
    }
    deriving stock (Show, Eq, Functor)
