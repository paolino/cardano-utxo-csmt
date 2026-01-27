{- |
Module      : Mithril.STM.SerializationSpec
Description : Tests for binary serialization
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

Tests for roundtrip serialization of all STM types.
-}
module Mithril.STM.SerializationSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Serialize (runGet, runPut)
import Test.Hspec
import Test.QuickCheck

import Mithril.STM.Crypto (BlsOps (..), HashOps (..))
import Mithril.STM.Parameters (Parameters (..))
import Mithril.STM.Serialization
    ( CryptoSizes (..)
    , getAggregateSignature
    , getAggregateVerificationKey
    , getMerkleBatchPath
    , getMerkleCommitment
    , getParameters
    , getRegistrationEntry
    , getSignedRegistration
    , getSingleSignature
    , putAggregateSignature
    , putAggregateVerificationKey
    , putMerkleBatchPath
    , putMerkleCommitment
    , putParameters
    , putRegistrationEntry
    , putSignedRegistration
    , putSingleSignature
    )
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

spec :: Spec
spec = do
    describe "Parameters" $ do
        it "roundtrips mainnet parameters" $ do
            let params =
                    Parameters
                        { paramM = 20973
                        , paramK = 2422
                        , paramPhiF = 0.2
                        }
            let bytes = runPut $ putParameters params
            runGet getParameters bytes `shouldBe` Right params

        it "serializes to 24 bytes" $ do
            let params = Parameters 100 50 0.5
            BS.length (runPut $ putParameters params) `shouldBe` 24

        it "roundtrips arbitrary parameters"
            $ property
            $ \m k phiF ->
                let params =
                        Parameters
                            { paramM = m
                            , paramK = k
                            , paramPhiF = abs phiF
                            }
                    bytes = runPut $ putParameters params
                in  runGet getParameters bytes === Right params

    describe "MerkleCommitment" $ do
        it "roundtrips" $ do
            let commitment =
                    MerkleCommitment
                        { mcRoot = BS.replicate 32 0xAB
                        , mcNrLeaves = 1000
                        }
            let bytes = runPut $ putMerkleCommitment mockHashOps commitment
            runGet (getMerkleCommitment testSizes mockHashOps) bytes
                `shouldBe` Right commitment

        it "serializes to 36 bytes (32 hash + 4 count)" $ do
            let commitment = MerkleCommitment (BS.replicate 32 0) 0
            BS.length (runPut $ putMerkleCommitment mockHashOps commitment)
                `shouldBe` 36

    describe "MerkleBatchPath" $ do
        it "roundtrips empty path" $ do
            let path = MerkleBatchPath [] []
            let bytes = runPut $ putMerkleBatchPath mockHashOps path
            runGet (getMerkleBatchPath testSizes mockHashOps) bytes
                `shouldBe` Right path

        it "roundtrips path with data" $ do
            let path =
                    MerkleBatchPath
                        { mbpSiblings =
                            [ BS.replicate 32 0x01
                            , BS.replicate 32 0x02
                            ]
                        , mbpIndices = [0, 1, 2]
                        }
            let bytes = runPut $ putMerkleBatchPath mockHashOps path
            runGet (getMerkleBatchPath testSizes mockHashOps) bytes
                `shouldBe` Right path

    describe "RegistrationEntry" $ do
        it "roundtrips" $ do
            let entry =
                    RegistrationEntry
                        { reVerificationKey = BS.replicate 96 0xBB
                        , reStake = 1000000
                        }
            let bytes = runPut $ putRegistrationEntry mockBlsOps entry
            runGet (getRegistrationEntry testSizes mockBlsOps) bytes
                `shouldBe` Right entry

    describe "SingleSignature" $ do
        it "roundtrips with empty indices" $ do
            let sig =
                    SingleSignature
                        { ssSignature = BS.replicate 48 0xCC
                        , ssIndices = []
                        }
            let bytes = runPut $ putSingleSignature mockBlsOps sig
            runGet (getSingleSignature testSizes mockBlsOps) bytes
                `shouldBe` Right sig

        it "roundtrips with indices" $ do
            let sig =
                    SingleSignature
                        { ssSignature = BS.replicate 48 0x11
                        , ssIndices = [0, 5, 10, 100]
                        }
            let bytes = runPut $ putSingleSignature mockBlsOps sig
            runGet (getSingleSignature testSizes mockBlsOps) bytes
                `shouldBe` Right sig

    describe "SignedRegistration" $ do
        it "roundtrips" $ do
            let sr = testSignedRegistration
            let bytes = runPut $ putSignedRegistration mockBlsOps sr
            runGet (getSignedRegistration testSizes mockBlsOps) bytes
                `shouldBe` Right sr

    describe "AggregateVerificationKey" $ do
        it "roundtrips" $ do
            let avk =
                    AggregateVerificationKey
                        { avkMerkleCommitment =
                            MerkleCommitment (BS.replicate 32 0xAA) 500
                        , avkTotalStake = 1000000000
                        }
            let bytes = runPut $ putAggregateVerificationKey mockHashOps avk
            runGet (getAggregateVerificationKey testSizes mockHashOps) bytes
                `shouldBe` Right avk

    describe "AggregateSignature" $ do
        it "roundtrips Concatenation variant" $ do
            let aggSig = testAggregateSignature
            let bytes =
                    runPut
                        $ putAggregateSignature mockHashOps mockBlsOps aggSig
            runGet
                (getAggregateSignature testSizes mockHashOps mockBlsOps)
                bytes
                `shouldBe` Right aggSig

        it "starts with 0x00 tag for Concatenation" $ do
            let aggSig = testAggregateSignature
            let bytes =
                    runPut
                        $ putAggregateSignature mockHashOps mockBlsOps aggSig
            BS.head bytes `shouldBe` 0x00

        it "rejects invalid tag" $ do
            let bytes = BS.singleton 0xFF -- Invalid tag
            case runGet
                (getAggregateSignature testSizes mockHashOps mockBlsOps)
                bytes of
                Left err -> err `shouldContain` "Invalid"
                Right _ -> expectationFailure "Should have failed"

-- ============================================================================
-- Test Data
-- ============================================================================

testSizes :: CryptoSizes
testSizes =
    CryptoSizes
        { sigSize = 48
        , vkSize = 96
        , hash256Size = 32
        }

testSignedRegistration :: SignedRegistration ByteString ByteString
testSignedRegistration =
    SignedRegistration
        { srSignature =
            SingleSignature
                { ssSignature = BS.replicate 48 0x22
                , ssIndices = [1, 2, 3]
                }
        , srRegistration =
            RegistrationEntry
                { reVerificationKey = BS.replicate 96 0x33
                , reStake = 500000
                }
        }

testAggregateSignature
    :: AggregateSignature ByteString ByteString ByteString
testAggregateSignature =
    Concatenation
        $ ConcatenationProof
            { cpSignatures = [testSignedRegistration]
            , cpBatchPath =
                MerkleBatchPath
                    { mbpSiblings = [BS.replicate 32 0x44]
                    , mbpIndices = [0]
                    }
            }

-- ============================================================================
-- Mock Crypto Operations
-- ============================================================================

mockHashOps :: HashOps ByteString ByteString
mockHashOps =
    HashOps
        { blake2b256 = id
        , blake2b512 = id
        , hash256ToBytes = id
        , hash512ToBytes = id
        , bytesToHash256 = Just
        , bytesToHash512 = Just
        }

mockBlsOps :: BlsOps ByteString ByteString
mockBlsOps =
    BlsOps
        { blsVerifyAggregate = \_ _ -> True
        , blsDeserializeSig = Just
        , blsDeserializeVk = Just
        , blsSerializeSig = id
        , blsSerializeVk = id
        }
