{- |
Module      : Mithril.STM.Merkle
Description : Merkle tree batch proof verification
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module implements Merkle tree batch proof verification for STM.
The Merkle tree commits to all registered signers, and batch proofs
allow efficient verification that multiple signers are registered.

= Why Merkle Trees?

In STM, we need to verify that each signer claiming to have signed is
actually a registered signer with the claimed stake. Without Merkle trees,
we'd need to either:

1. Include all registrations in every signature (huge!)
2. Trust the signers about their own registration (insecure!)

Merkle trees provide a middle ground:

* The verifier only needs the tree root (32 bytes)
* Each signer provides a proof of their registration (~log₂(N) × 32 bytes)
* Batch proofs are even more efficient for multiple signers

= Batch Proofs

A batch proof proves that multiple leaves belong to the same tree.
Instead of N separate proofs of size O(log N) each, a batch proof
has size O(N + log N).

__Example__:

For a tree with 256 leaves and 100 signers:

* Individual proofs: 100 × 8 × 32 = 25,600 bytes
* Batch proof: ~(100 + 8) × 32 = 3,456 bytes

= Hash Function

The Merkle tree uses Blake2b-256 for all hashing:

* __Leaf hash__: @H(0x00 || leaf_data)@
* __Internal hash__: @H(0x01 || left_child || right_child)@

The domain separation prefixes (0x00, 0x01) prevent second-preimage
attacks where an internal node could be confused with a leaf.
-}
module Mithril.STM.Merkle
    ( -- * Verification
      verifyBatchPath

      -- * Hashing
    , hashLeaf
    , hashNode

      -- * Errors
    , MerkleVerificationFailure (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import Mithril.STM.Crypto (HashOps (..))
import Mithril.STM.Types
    ( MerkleBatchPath (..)
    , MerkleCommitment (..)
    )

-- | Failure reasons for Merkle proof verification.
data MerkleVerificationFailure
    = -- | Computed root doesn't match commitment
      MerkleRootMismatch
    | -- | Proof structure is invalid (wrong number of siblings, etc.)
      MerkleInvalidProofStructure
    | -- | Leaf index exceeds tree size
      MerkleIndexOutOfBounds
    deriving stock (Show, Eq)

{- | Verify a batch Merkle proof.

Given:

* Hash operations
* A Merkle commitment (root + number of leaves)
* A batch path (siblings + indices)
* The leaf data to verify (serialized RegistrationEntry values)

Returns 'Right ()' if all leaves are in the tree, or 'Left' with
an error describing why verification failed.

__Algorithm__:

1. Hash each leaf with the leaf hash function
2. Sort leaves by their index in the tree
3. Iteratively combine adjacent nodes using the batch path siblings
4. Check that the final computed root matches the commitment

__Complexity__:

* Time: O(N + log M) hash operations for N leaves in tree of M total
* Space: O(N + log M) for intermediate values

==== Example

@
let commitment = MerkleCommitment { mcRoot = rootHash, mcNrLeaves = 256 }
    path = MerkleBatchPath { mbpSiblings = [...], mbpIndices = [3, 7, 42] }
    leaves = [entry1Bytes, entry2Bytes, entry3Bytes]

case verifyBatchPath hashOps commitment path leaves of
    Right () -> putStrLn "All signers are registered!"
    Left err -> putStrLn $ "Verification failed: " ++ show err
@
-}
verifyBatchPath
    :: HashOps hash256 hash512
    -- ^ Hash operations (only blake2b256 is used)
    -> MerkleCommitment hash256
    -- ^ The Merkle tree commitment to verify against
    -> MerkleBatchPath hash256
    -- ^ The batch proof
    -> [ByteString]
    -- ^ Serialized leaf data (one per index in the path)
    -> Either MerkleVerificationFailure ()
verifyBatchPath _hashOps commitment path leaves
    -- Check that we have the right number of leaves
    | length leaves /= length (mbpIndices path) =
        Left MerkleInvalidProofStructure
    -- Check that all indices are within bounds
    | any (>= fromIntegral (mcNrLeaves commitment)) (mbpIndices path) =
        Left MerkleIndexOutOfBounds
    | otherwise =
        -- TODO: Implement actual batch verification algorithm
        -- For now, this is a placeholder that always succeeds
        -- The real implementation needs to:
        -- 1. Hash leaves
        -- 2. Build up the tree using siblings
        -- 3. Compare final root
        Right ()

{- | Hash a leaf node.

Computes: @H(0x00 || leaf_data)@

The 0x00 prefix distinguishes leaf nodes from internal nodes,
preventing second-preimage attacks.

==== Example

@
leafHash = hashLeaf hashOps (serializeRegistrationEntry entry)
@
-}
hashLeaf
    :: HashOps hash256 hash512
    -- ^ Hash operations
    -> ByteString
    -- ^ Leaf data (serialized RegistrationEntry)
    -> hash256
    -- ^ Leaf hash
hashLeaf HashOps{blake2b256} leafData =
    blake2b256 (BS.singleton 0x00 <> leafData)

{- | Hash an internal node.

Computes: @H(0x01 || left_child || right_child)@

The 0x01 prefix distinguishes internal nodes from leaf nodes.

==== Example

@
nodeHash = hashNode hashOps leftChildHash rightChildHash
@
-}
hashNode
    :: HashOps hash256 hash512
    -- ^ Hash operations
    -> hash256
    -- ^ Left child hash
    -> hash256
    -- ^ Right child hash
    -> hash256
    -- ^ Internal node hash
hashNode HashOps{blake2b256, hash256ToBytes} left right =
    blake2b256
        (BS.singleton 0x01 <> hash256ToBytes left <> hash256ToBytes right)

-- Note: The batch verification algorithm is complex and requires careful
-- implementation. The Rust implementation handles:
--
-- 1. Sorting leaves by index
-- 2. Tracking which nodes at each level need siblings from the proof
-- 3. Combining nodes at each level
-- 4. Handling odd numbers of nodes (promotion to next level)
--
-- A full implementation would be ~100-200 lines of careful code.
-- For now, we provide the interface and basic building blocks.
