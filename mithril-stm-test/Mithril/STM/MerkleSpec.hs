{- |
Module      : Mithril.STM.MerkleSpec
Description : Tests for Merkle tree batch proof verification
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

Tests for Merkle tree utilities and batch proof verification.
-}
module Mithril.STM.MerkleSpec (spec) where

import Data.Bits (xor, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

import Mithril.STM.Crypto (HashOps (..))
import Mithril.STM.Merkle
    ( MerkleVerificationFailure (..)
    , hashLeaf
    , hashNode
    , isLeftChild
    , leafToNodeIndex
    , nextPowerOfTwo
    , parent
    , sibling
    , verifyBatchPath
    )
import Mithril.STM.Types
    ( MerkleBatchPath (..)
    , MerkleCommitment (..)
    )

spec :: Spec
spec = do
    describe "Tree navigation utilities" $ do
        describe "parent" $ do
            it "parent(1) = 0" $ parent 1 `shouldBe` 0
            it "parent(2) = 0" $ parent 2 `shouldBe` 0
            it "parent(3) = 1" $ parent 3 `shouldBe` 1
            it "parent(4) = 1" $ parent 4 `shouldBe` 1
            it "parent(5) = 2" $ parent 5 `shouldBe` 2
            it "parent(6) = 2" $ parent 6 `shouldBe` 2

        describe "sibling" $ do
            it "sibling(1) = 2 (left child's sibling is right)"
                $ sibling 1
                `shouldBe` 2
            it "sibling(2) = 1 (right child's sibling is left)"
                $ sibling 2
                `shouldBe` 1
            it "sibling(3) = 4" $ sibling 3 `shouldBe` 4
            it "sibling(4) = 3" $ sibling 4 `shouldBe` 3

            it "sibling is involutive: sibling(sibling(i)) = i"
                $ property
                $ \(Positive i) -> sibling (sibling i) == i

        describe "isLeftChild" $ do
            it "odd indices are left children" $ do
                isLeftChild 1 `shouldBe` True
                isLeftChild 3 `shouldBe` True
                isLeftChild 5 `shouldBe` True

            it "even indices are right children" $ do
                isLeftChild 2 `shouldBe` False
                isLeftChild 4 `shouldBe` False
                isLeftChild 6 `shouldBe` False

        describe "nextPowerOfTwo" $ do
            it "nextPowerOfTwo 0 = 1" $ nextPowerOfTwo 0 `shouldBe` 1
            it "nextPowerOfTwo 1 = 1" $ nextPowerOfTwo 1 `shouldBe` 1
            it "nextPowerOfTwo 2 = 2" $ nextPowerOfTwo 2 `shouldBe` 2
            it "nextPowerOfTwo 3 = 4" $ nextPowerOfTwo 3 `shouldBe` 4
            it "nextPowerOfTwo 4 = 4" $ nextPowerOfTwo 4 `shouldBe` 4
            it "nextPowerOfTwo 5 = 8" $ nextPowerOfTwo 5 `shouldBe` 8
            it "nextPowerOfTwo 100 = 128" $ nextPowerOfTwo 100 `shouldBe` 128
            it "nextPowerOfTwo 256 = 256" $ nextPowerOfTwo 256 `shouldBe` 256

            it "result is always >= input"
                $ property
                $ \(Positive n) ->
                    let n' = fromIntegral (n :: Int) `mod` 10000 + 1
                    in  nextPowerOfTwo n' >= n'

            it "result is always a power of 2"
                $ property
                $ \(Positive n) ->
                    let n' = fromIntegral (n :: Int) `mod` 10000 + 1
                        p = nextPowerOfTwo n'
                    in  p .&. (p - 1) == 0

        describe "leafToNodeIndex" $ do
            it "for 4 leaves, leaf 0 -> node 3"
                $ leafToNodeIndex 4 0
                `shouldBe` 3
            it "for 4 leaves, leaf 1 -> node 4"
                $ leafToNodeIndex 4 1
                `shouldBe` 4
            it "for 4 leaves, leaf 2 -> node 5"
                $ leafToNodeIndex 4 2
                `shouldBe` 5
            it "for 4 leaves, leaf 3 -> node 6"
                $ leafToNodeIndex 4 3
                `shouldBe` 6

            it "for 8 leaves, leaf 0 -> node 7"
                $ leafToNodeIndex 8 0
                `shouldBe` 7

    describe "Hashing functions" $ do
        describe "hashLeaf" $ do
            it "prepends 0x00 domain separator" $ do
                let leaf = "test data"
                    hash = hashLeaf mockHashOps leaf
                -- The mock hash just returns the input, so we can check prefix
                hash `shouldBe` (BS.singleton 0x00 <> leaf)

            it "different leaves produce different hashes" $ do
                let leaf1 = "leaf one"
                    leaf2 = "leaf two"
                hashLeaf mockHashOps leaf1 `shouldNotBe` hashLeaf mockHashOps leaf2

        describe "hashNode" $ do
            it "prepends 0x01 domain separator" $ do
                let left = "left hash"
                    right = "right hash"
                    hash = hashNode mockHashOps left right
                -- Mock hash returns input, check structure
                hash `shouldBe` (BS.singleton 0x01 <> left <> right)

            it "order matters (not commutative)" $ do
                let h1 = "hash1"
                    h2 = "hash2"
                hashNode mockHashOps h1 h2 `shouldNotBe` hashNode mockHashOps h2 h1

    describe "verifyBatchPath" $ do
        describe "input validation" $ do
            it "rejects mismatched lengths" $ do
                let commitment = MerkleCommitment "root" 4
                    path = MerkleBatchPath [] [0, 1] -- 2 indices
                    leaves = ["leaf1"] -- 1 leaf
                verifyBatchPath mockHashOps commitment path leaves
                    `shouldBe` Left MerkleInvalidProofStructure

            it "rejects out-of-bounds indices" $ do
                let commitment = MerkleCommitment "root" 4 -- 4 leaves
                    path = MerkleBatchPath [] [5] -- Index 5 >= 4
                    leaves = ["leaf"]
                verifyBatchPath mockHashOps commitment path leaves
                    `shouldBe` Left MerkleIndexOutOfBounds

            it "rejects unsorted indices" $ do
                let commitment = MerkleCommitment "root" 4
                    path = MerkleBatchPath [] [2, 1] -- Not sorted
                    leaves = ["leaf1", "leaf2"]
                verifyBatchPath mockHashOps commitment path leaves
                    `shouldBe` Left MerkleIndicesNotSorted

            it "accepts empty proof" $ do
                let commitment = MerkleCommitment "root" 4
                    path = MerkleBatchPath [] []
                    leaves = []
                verifyBatchPath mockHashOps commitment path leaves
                    `shouldBe` Right ()

        describe "single leaf verification" $ do
            it "verifies single leaf with correct proof" $ do
                -- Build a 4-leaf tree manually and verify one leaf
                --
                --        [root]
                --       /      \
                --     [n1]    [n2]
                --    /    \   /   \
                --  [L0] [L1] [L2] [L3]
                --
                -- To prove L0, we need: L1 (sibling), n2 (uncle)
                let leaf0 = "leaf0"
                    leaf1 = "leaf1"
                    leaf2 = "leaf2"
                    leaf3 = "leaf3"

                    -- Hash leaves
                    h0 = hashLeaf realHashOps leaf0
                    h1 = hashLeaf realHashOps leaf1
                    h2 = hashLeaf realHashOps leaf2
                    h3 = hashLeaf realHashOps leaf3

                    -- Hash internal nodes
                    n1 = hashNode realHashOps h0 h1
                    n2 = hashNode realHashOps h2 h3
                    root = hashNode realHashOps n1 n2

                    -- Proof for L0: sibling is L1, uncle is n2
                    commitment = MerkleCommitment root 4
                    path = MerkleBatchPath [h1, n2] [0]
                    leaves = [leaf0]

                verifyBatchPath realHashOps commitment path leaves
                    `shouldBe` Right ()

        describe "adjacent leaves verification" $ do
            it "verifies two adjacent leaves efficiently" $ do
                -- Prove L0 and L1 together (they share parent n1)
                -- We only need n2 as sibling (not individual siblings)
                let leaf0 = "leaf0"
                    leaf1 = "leaf1"
                    leaf2 = "leaf2"
                    leaf3 = "leaf3"

                    h0 = hashLeaf realHashOps leaf0
                    h1 = hashLeaf realHashOps leaf1
                    h2 = hashLeaf realHashOps leaf2
                    h3 = hashLeaf realHashOps leaf3

                    n1 = hashNode realHashOps h0 h1
                    n2 = hashNode realHashOps h2 h3
                    root = hashNode realHashOps n1 n2

                    -- Proof for L0 and L1: they combine to n1, need n2
                    commitment = MerkleCommitment root 4
                    path = MerkleBatchPath [n2] [0, 1]
                    leaves = [leaf0, leaf1]

                verifyBatchPath realHashOps commitment path leaves
                    `shouldBe` Right ()

        describe "root mismatch detection" $ do
            it "rejects proof when root doesn't match" $ do
                let leaf0 = "leaf0"
                    leaf1 = "leaf1"
                    leaf2 = "leaf2"
                    leaf3 = "leaf3"

                    h0 = hashLeaf realHashOps leaf0
                    h1 = hashLeaf realHashOps leaf1
                    h2 = hashLeaf realHashOps leaf2
                    h3 = hashLeaf realHashOps leaf3

                    n1 = hashNode realHashOps h0 h1
                    n2 = hashNode realHashOps h2 h3
                    _root = hashNode realHashOps n1 n2
                    wrongRoot = "wrong root"

                    commitment = MerkleCommitment wrongRoot 4
                    path = MerkleBatchPath [h1, n2] [0]
                    leaves = [leaf0]

                verifyBatchPath realHashOps commitment path leaves
                    `shouldBe` Left MerkleRootMismatch

-- ============================================================================
-- Mock and Real Hash Operations
-- ============================================================================

-- | Mock hash operations that just return the input (for testing structure)
mockHashOps :: HashOps ByteString ByteString
mockHashOps =
    HashOps
        { blake2b256 = id -- Identity for testing
        , blake2b512 = id
        , hash256ToBytes = id
        , hash512ToBytes = id
        , bytesToHash256 = Just
        , bytesToHash512 = Just
        }

{- | Real-ish hash operations using simple hashing for tests
Uses a simple hash function (not cryptographic, just for testing)
-}
realHashOps :: HashOps ByteString ByteString
realHashOps =
    HashOps
        { blake2b256 = simpleHash
        , blake2b512 = simpleHash
        , hash256ToBytes = id
        , hash512ToBytes = id
        , bytesToHash256 = Just
        , bytesToHash512 = Just
        }
  where
    -- Simple hash: XOR fold + length encoding (NOT CRYPTOGRAPHIC!)
    -- Just deterministic enough for testing Merkle structure
    simpleHash :: ByteString -> ByteString
    simpleHash bs =
        let len = BS.length bs
            xorFold = BS.foldl' xor 0 bs
            -- Create a "hash" that depends on content and length
            hashBytes =
                [ xorFold
                , fromIntegral (len `mod` 256)
                , fromIntegral ((len `div` 256) `mod` 256)
                , xorFold `xor` 0x5A
                ]
                    ++ replicate 28 (xorFold `xor` fromIntegral len)
        in  BS.pack hashBytes
