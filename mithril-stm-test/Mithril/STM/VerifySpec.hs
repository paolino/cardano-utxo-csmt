{- |
Module      : Mithril.STM.VerifySpec
Description : Tests for STM aggregate signature verification
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

Tests for the main STM verification flow using mock crypto operations.
-}
module Mithril.STM.VerifySpec (spec) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word64)
import Test.Hspec

import Mithril.STM.Crypto (BlsOps (..), HashOps (..))
import Mithril.STM.Parameters (Parameters (..), mainnetParameters)
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
import Mithril.STM.Verify
    ( VerificationFailure (..)
    , transformMessage
    , verify
    )

spec :: Spec
spec = do
    describe "transformMessage" $ do
        it "includes merkle root in transformed message" $ do
            let commitment = MerkleCommitment "merkle_root_hash" 100
                message = "original message"
                transformed = transformMessage mockHashOps commitment message
            -- The transformed message should be different from original
            transformed `shouldNotBe` message

        it "is deterministic" $ do
            let commitment = MerkleCommitment "root" 10
                message = "test"
            transformMessage mockHashOps commitment message
                `shouldBe` transformMessage mockHashOps commitment message

        it "changes with different roots" $ do
            let commitment1 = MerkleCommitment "root1" 10
                commitment2 = MerkleCommitment "root2" 10
                message = "test"
            transformMessage mockHashOps commitment1 message
                `shouldNotBe` transformMessage mockHashOps commitment2 message

    describe "verify" $ do
        describe "with mock BLS (always passes)" $ do
            it "succeeds for valid-looking signature" $ do
                let params = testParameters
                    avk = testAggregateVerificationKey
                    message = "test message"
                    aggSig = testAggregateSignature

                -- With mock BLS that always succeeds and empty proof,
                -- verification should pass
                verify mockHashOps mockBlsOps params avk message aggSig
                    `shouldBe` Right ()

        describe "with mock BLS (always fails)" $ do
            it "fails BLS verification" $ do
                let params = testParameters
                    avk = testAggregateVerificationKey
                    message = "test message"
                    aggSig = testAggregateSignature

                verify mockHashOps failingBlsOps params avk message aggSig
                    `shouldBe` Left BlsVerificationFailed

        describe "error cases" $ do
            it "reports insufficient signatures when quorum not met" $ do
                -- With k=10 but only 5 unique indices, should fail
                let params = testParameters{paramK = 10}
                    avk = testAggregateVerificationKey
                    message = "test"
                    -- Signature with only 5 indices (below k=10)
                    sig =
                        testAggregateSignatureWithIndices
                            [[0, 1, 2, 3, 4]]
                            params

                case verify mockHashOps mockBlsOps params avk message sig of
                    Left (InsufficientSignatures got need) -> do
                        got `shouldBe` 5
                        need `shouldBe` 10
                    other ->
                        expectationFailure
                            $ "Expected InsufficientSignatures, got: " ++ show other

            it "reports index out of bounds" $ do
                let params = testParameters{paramM = 100}
                    avk = testAggregateVerificationKey
                    message = "test"
                    -- Index 150 > m=100
                    sig = testAggregateSignatureWithIndices [[150]] params

                case verify mockHashOps mockBlsOps params avk message sig of
                    Left (IndexOutOfBounds idx m) -> do
                        idx `shouldBe` 150
                        m `shouldBe` 100
                    other ->
                        expectationFailure $ "Expected IndexOutOfBounds, got: " ++ show other

            it "reports duplicate indices" $ do
                let params = testParameters
                    avk = testAggregateVerificationKey
                    message = "test"
                    -- Two signers claiming same index 5
                    sig = testAggregateSignatureWithIndices [[5], [5]] params

                case verify mockHashOps mockBlsOps params avk message sig of
                    Left (DuplicateIndex idx) -> idx `shouldBe` 5
                    other ->
                        expectationFailure $ "Expected DuplicateIndex, got: " ++ show other

    describe "mainnetParameters" $ do
        it "has expected values" $ do
            paramM mainnetParameters `shouldBe` 20973
            paramK mainnetParameters `shouldBe` 2422
            paramPhiF mainnetParameters `shouldBe` 0.2

-- ============================================================================
-- Test Data
-- ============================================================================

-- | Test parameters with small values for easy testing
testParameters :: Parameters
testParameters =
    Parameters
        { paramM = 1000 -- Max lottery index
        , paramK = 5 -- Quorum (need at least 5 unique indices)
        , paramPhiF = 0.2
        }

-- | Test aggregate verification key
testAggregateVerificationKey :: AggregateVerificationKey ByteString
testAggregateVerificationKey =
    AggregateVerificationKey
        { avkMerkleCommitment =
            MerkleCommitment
                { mcRoot = "test_merkle_root"
                , mcNrLeaves = 10
                }
        , avkTotalStake = 1000000
        }

-- | Test aggregate signature with enough indices to pass quorum
testAggregateSignature
    :: AggregateSignature ByteString ByteString ByteString
testAggregateSignature =
    testAggregateSignatureWithIndices
        [[0, 1, 2], [3, 4, 5, 6, 7]]
        testParameters

-- | Build a test aggregate signature with specific indices
testAggregateSignatureWithIndices
    :: [[Word64]]
    -- ^ List of index lists, one per signer
    -> Parameters
    -> AggregateSignature ByteString ByteString ByteString
testAggregateSignatureWithIndices indexLists _params =
    Concatenation
        $ ConcatenationProof
            { cpSignatures = map mkSignedReg indexLists
            , cpBatchPath =
                MerkleBatchPath
                    { mbpSiblings = []
                    , mbpIndices = [] -- Empty for simplified testing
                    }
            }
  where
    mkSignedReg indices =
        SignedRegistration
            { srSignature =
                SingleSignature
                    { ssSignature = "mock_signature"
                    , ssIndices = indices
                    }
            , srRegistration =
                RegistrationEntry
                    { reVerificationKey = "mock_vk"
                    , reStake = 100000 -- 10% of total stake
                    }
            }

-- ============================================================================
-- Mock Crypto Operations
-- ============================================================================

{- | Mock hash operations for testing.

The blake2b512 returns all zeros, which makes the lottery always pass
(since 0 / 2^512 = 0 < φ(w) for any positive stake).

The blake2b256 uses a simple deterministic hash for Merkle operations.
-}
mockHashOps :: HashOps ByteString ByteString
mockHashOps =
    HashOps
        { blake2b256 = simpleHash256
        , blake2b512 = lotteryWinHash
        , hash256ToBytes = id
        , hash512ToBytes = id
        , bytesToHash256 = Just
        , bytesToHash512 = Just
        }
  where
    -- Simple deterministic 32-byte hash for Merkle operations
    simpleHash256 bs =
        let len = BS.length bs
            xorFold = BS.foldl' xor 0 bs
        in  BS.pack $ [xorFold, fromIntegral len] ++ replicate 30 xorFold

    -- Returns all zeros (64 bytes) - this will always win the lottery
    -- since 0 / 2^512 = 0 which is less than any positive phi(w)
    lotteryWinHash _ = BS.replicate 64 0

-- | Mock BLS operations that always succeed
mockBlsOps :: BlsOps ByteString ByteString
mockBlsOps =
    BlsOps
        { blsVerifyAggregate = \_ _ -> True -- Always succeeds
        , blsDeserializeSig = Just
        , blsDeserializeVk = Just
        , blsSerializeSig = id
        , blsSerializeVk = id
        }

-- | Mock BLS operations that always fail verification
failingBlsOps :: BlsOps ByteString ByteString
failingBlsOps =
    mockBlsOps
        { blsVerifyAggregate = \_ _ -> False
        }
