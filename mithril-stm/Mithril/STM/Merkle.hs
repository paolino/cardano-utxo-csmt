{-# LANGUAGE BangPatterns #-}

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

= Tree Structure

The Merkle tree is stored as a perfect binary tree in heap layout:

@
          [0]              <- root (index 0)
         /    \\
      [1]      [2]         <- level 1
     /   \\    /   \\
   [3]  [4]  [5]  [6]      <- level 2 (leaves for 4-leaf tree)
@

For N leaves, the tree has @2N - 1@ total nodes.
Leaf indices are in the range @[N-1, 2N-2]@ (0-indexed).

= Hash Function

The Merkle tree uses Blake2b-256 for all hashing:

* __Leaf hash__: @H(0x00 || leaf_data)@
* __Internal hash__: @H(0x01 || left_child || right_child)@

The domain separation prefixes (0x00, 0x01) prevent second-preimage
attacks where an internal node could be confused with a leaf.

= Batch Verification Algorithm

The "Octopus" algorithm efficiently verifies multiple leaves:

1. Convert leaf indices to tree node positions
2. Hash all leaves
3. Level by level, moving toward the root:
   - Group nodes by their parent
   - For adjacent siblings (both in our set), combine directly
   - For lone nodes, fetch sibling hash from the proof
   - Compute parent hashes
4. Final hash should equal the committed root

This avoids redundant sibling lookups when proving adjacent leaves.
-}
module Mithril.STM.Merkle
    ( -- * Verification
      verifyBatchPath

      -- * Hashing
    , hashLeaf
    , hashNode

      -- * Errors
    , MerkleVerificationFailure (..)

      -- * Tree Utilities (exported for testing)
    , parent
    , leftChild
    , rightChild
    , sibling
    , isLeftChild
    , nextPowerOfTwo
    , leafToNodeIndex
    ) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32, Word64)

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
    | -- | Indices not sorted in ascending order
      MerkleIndicesNotSorted
    | -- | Proof consumed incorrectly (internal error)
      MerkleProofExhausted
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

1. Validate inputs (lengths match, indices sorted, in bounds)
2. Convert leaf indices to internal tree node positions
3. Hash each leaf
4. Level by level, traverse toward the root:
   - For each node, find its sibling
   - If sibling is in our working set, combine them
   - Otherwise, take sibling hash from the proof
   - Compute parent hash
5. Final hash should equal the committed root

__Complexity__:

* Time: O(N × log M) hash operations for N leaves in tree of M total
* Space: O(N) for working set of hashes

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
    :: forall hash256 hash512
     . (Eq hash256)
    => HashOps hash256 hash512
    -- ^ Hash operations (only blake2b256 is used)
    -> MerkleCommitment hash256
    -- ^ The Merkle tree commitment to verify against
    -> MerkleBatchPath hash256
    -- ^ The batch proof
    -> [ByteString]
    -- ^ Serialized leaf data (one per index in the path)
    -> Either MerkleVerificationFailure ()
verifyBatchPath hashOps commitment path leaves = do
    -- Step 1: Validate inputs
    let nrLeaves = mcNrLeaves commitment
        indices = mbpIndices path
        siblings = mbpSiblings path

    -- Check lengths match
    if length leaves /= length indices
        then Left MerkleInvalidProofStructure
        else pure ()

    -- Check indices are sorted (required for the algorithm)
    if not (isSorted indices)
        then Left MerkleIndicesNotSorted
        else pure ()

    -- Check all indices are within bounds
    if any (>= fromIntegral nrLeaves) indices
        then Left MerkleIndexOutOfBounds
        else pure ()

    -- Handle empty case
    if null leaves
        then Right () -- Nothing to verify
        else do
            -- Step 2: Convert leaf indices to tree node positions
            -- In a heap-layout tree with N leaves, leaves are at positions
            -- [treeSize - nrLeaves, treeSize - 1] where treeSize = 2 * nextPow2 - 1
            let treeNrLeaves = nextPowerOfTwo nrLeaves
                nodeIndices =
                    map (leafToNodeIndex treeNrLeaves . fromIntegral) indices

            -- Step 3: Hash all leaves
            let leafHashes = map (hashLeaf hashOps) leaves

            -- Build initial working set: Map from node index to hash
            let initialSet :: Map Word64 hash256
                initialSet = Map.fromList (zip nodeIndices leafHashes)

            -- Step 4: Traverse up the tree, consuming siblings from proof
            computedRoot <-
                traverseToRoot hashOps treeNrLeaves siblings initialSet

            -- Step 5: Compare with committed root
            if computedRoot == mcRoot commitment
                then Right ()
                else Left MerkleRootMismatch

{- | Traverse from leaves to root, computing parent hashes.

At each level:
1. Group nodes by their parent
2. For nodes whose sibling is also in our set, combine directly
3. For lone nodes, consume a sibling from the proof
4. Compute parent hashes and continue to next level

Returns the computed root hash, or an error if proof is malformed.
-}
traverseToRoot
    :: forall hash256 hash512
     . HashOps hash256 hash512
    -> Word32
    -- ^ Number of leaves (power of 2)
    -> [hash256]
    -- ^ Remaining siblings from proof
    -> Map Word64 hash256
    -- ^ Current working set (node index -> hash)
    -> Either MerkleVerificationFailure hash256
traverseToRoot hashOps _treeNrLeaves = go
  where
    go
        :: [hash256]
        -> Map Word64 hash256
        -> Either MerkleVerificationFailure hash256
    go !remainingSiblings !workingSet
        -- If we have exactly one node at index 0, we've reached the root
        | Map.size workingSet == 1
        , Just rootHash <- Map.lookup 0 workingSet =
            Right rootHash
        -- If working set is empty, something went wrong
        | Map.null workingSet =
            Left MerkleInvalidProofStructure
        -- Otherwise, compute parent level
        | otherwise = do
            (newSiblings, parentSet) <-
                computeParentLevel hashOps remainingSiblings workingSet
            go newSiblings parentSet

{- | Compute the parent level from the current working set.

For each unique parent index in the current set:
- If both children are in the set, combine them
- If only one child is in the set, consume a sibling from proof

Returns (remaining siblings, parent working set).
-}
computeParentLevel
    :: forall hash256 hash512
     . HashOps hash256 hash512
    -> [hash256]
    -- ^ Available siblings from proof
    -> Map Word64 hash256
    -- ^ Current working set
    -> Either MerkleVerificationFailure ([hash256], Map Word64 hash256)
computeParentLevel hashOps siblings workingSet = do
    -- Get all node indices, sorted
    let nodeIndices = Map.keys workingSet

    -- Process nodes to compute parents, consuming siblings as needed
    processNodes hashOps siblings workingSet nodeIndices Map.empty

{- | Process nodes at current level to compute parent hashes.

Walks through sorted node indices:
- If current node and its sibling are both in working set, combine them
- Otherwise, consume a sibling hash from the proof
- Add parent hash to the result set
-}
processNodes
    :: forall hash256 hash512
     . HashOps hash256 hash512
    -> [hash256]
    -- ^ Remaining siblings
    -> Map Word64 hash256
    -- ^ Current level hashes
    -> [Word64]
    -- ^ Remaining node indices to process
    -> Map Word64 hash256
    -- ^ Accumulated parent hashes
    -> Either MerkleVerificationFailure ([hash256], Map Word64 hash256)
processNodes _ siblings _ [] parents =
    -- Done processing all nodes
    Right (siblings, parents)
processNodes hashOps siblings currentLevel (idx : rest) parents
    -- Skip if we already processed this node (as sibling of previous)
    | parent idx `Map.member` parents =
        processNodes hashOps siblings currentLevel rest parents
    | otherwise = do
        let sibIdx = sibling idx
            parentIdx = parent idx
            myHash = currentLevel Map.! idx -- Safe: idx is from Map.keys

        -- Check if sibling is in our working set
        case Map.lookup sibIdx currentLevel of
            Just sibHash -> do
                -- Both children present - combine them
                let parentHash =
                        if isLeftChild idx
                            then hashNode hashOps myHash sibHash
                            else hashNode hashOps sibHash myHash
                let parents' = Map.insert parentIdx parentHash parents
                -- Skip sibling in remaining indices (it's been processed)
                let rest' = filter (/= sibIdx) rest
                processNodes hashOps siblings currentLevel rest' parents'
            Nothing -> do
                -- Need sibling from proof
                case siblings of
                    [] -> Left MerkleProofExhausted
                    (sibHash : siblings') -> do
                        let parentHash =
                                if isLeftChild idx
                                    then hashNode hashOps myHash sibHash
                                    else hashNode hashOps sibHash myHash
                        let parents' = Map.insert parentIdx parentHash parents
                        processNodes hashOps siblings' currentLevel rest parents'

-- ============================================================================
-- Hashing Functions
-- ============================================================================

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

-- ============================================================================
-- Tree Navigation Utilities
-- ============================================================================

{- | Get parent index in heap layout.

In heap layout: @parent(i) = (i - 1) / 2@

@
     [0]        <- parent of 1 and 2
    /   \\
  [1]   [2]     <- parent of 3,4 and 5,6
  / \\   / \\
[3][4] [5][6]
@
-}
parent :: Word64 -> Word64
parent i = (i - 1) `div` 2

{- | Get left child index in heap layout.

@leftChild(i) = 2i + 1@
-}
leftChild :: Word64 -> Word64
leftChild i = 2 * i + 1

{- | Get right child index in heap layout.

@rightChild(i) = 2i + 2@
-}
rightChild :: Word64 -> Word64
rightChild i = 2 * i + 2

{- | Get sibling index in heap layout.

If index is even (right child), sibling is index - 1.
If index is odd (left child), sibling is index + 1.
-}
sibling :: Word64 -> Word64
sibling i
    | even i = i - 1 -- Right child, sibling is left
    | otherwise = i + 1 -- Left child, sibling is right

{- | Check if index is a left child.

Left children have odd indices in heap layout.
-}
isLeftChild :: Word64 -> Bool
isLeftChild i = odd i

{- | Convert a leaf index (0-based) to internal tree node index.

In a tree with N leaves (N = power of 2), leaves occupy
positions [N-1, 2N-2] in the heap layout.

@
leafToNodeIndex 4 0 = 3  -- First leaf
leafToNodeIndex 4 1 = 4  -- Second leaf
leafToNodeIndex 4 2 = 5  -- Third leaf
leafToNodeIndex 4 3 = 6  -- Fourth leaf
@
-}
leafToNodeIndex
    :: Word32
    -- ^ Number of leaves (must be power of 2)
    -> Word64
    -- ^ Leaf index (0-based, < nrLeaves)
    -> Word64
    -- ^ Tree node index
leafToNodeIndex nrLeaves leafIdx =
    fromIntegral nrLeaves - 1 + leafIdx

{- | Compute the next power of 2 >= n.

Used to determine tree size for a given number of leaves.
The Merkle tree is always a perfect binary tree, so we round
up to the next power of 2.

@
nextPowerOfTwo 1   = 1
nextPowerOfTwo 2   = 2
nextPowerOfTwo 3   = 4
nextPowerOfTwo 4   = 4
nextPowerOfTwo 5   = 8
nextPowerOfTwo 100 = 128
nextPowerOfTwo 256 = 256
@
-}
nextPowerOfTwo :: Word32 -> Word32
nextPowerOfTwo 0 = 1
nextPowerOfTwo n
    | n .&. (n - 1) == 0 = n -- Already a power of 2
    | otherwise = go 1
  where
    go p
        | p >= n = p
        | otherwise = go (p * 2)

-- | Check if a list is sorted in ascending order.
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : rest) = x <= y && isSorted (y : rest)
