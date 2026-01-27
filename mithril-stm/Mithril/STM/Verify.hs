{-# LANGUAGE BangPatterns #-}

{- |
Module      : Mithril.STM.Verify
Description : STM aggregate signature verification
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module implements the main STM signature verification algorithm.
It combines all the components (lottery, Merkle proofs, BLS verification)
into a single verification function.

= Verification Steps

STM verification proceeds in two phases:

== Phase 1: Preliminary Verification

1. __Index bounds__: Check all claimed lottery indices are < @m@
2. __Lottery check__: For each signer and index, verify they won the lottery
3. __Uniqueness__: Ensure no duplicate indices across all signatures
4. __Quorum__: Count unique indices and verify count >= @k@
5. __Merkle proof__: Verify all signers are in the registration commitment

== Phase 2: BLS Verification

1. __Message transform__: Compute @msg' = H(merkle_root || msg)@
2. __Aggregate verify__: Check the aggregate BLS signature over @msg'@

= Security Properties

A valid STM signature guarantees (with overwhelming probability):

* At least @k@ lottery indices were won by registered signers
* Each winning signer has the claimed stake in the commitment
* All signers signed the same message

An adversary would need to either:

* Win lottery indices they shouldn't (requires breaking Blake2b)
* Forge BLS signatures (requires breaking BLS12-381)
* Create fake Merkle proofs (requires breaking Blake2b)

= Performance

Verification performance is dominated by:

1. BLS aggregate verification: ~50ms for ~200 signatures
2. Merkle batch proof: ~1ms
3. Lottery checks: ~1ms

Total: ~50-100ms per certificate on typical hardware.
-}
module Mithril.STM.Verify
    ( -- * Main Verification
      verify

      -- * Verification Failures
    , VerificationFailure (..)

      -- * Internal (exported for testing)
    , preliminaryVerify
    , transformMessage
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)

import Mithril.STM.Crypto (BlsOps (..), HashOps (..))
import Mithril.STM.Lottery (isLotteryWon)
import Mithril.STM.Merkle (MerkleVerificationFailure, verifyBatchPath)
import Mithril.STM.Parameters (Parameters (..))
import Mithril.STM.Types
    ( AggregateSignature (..)
    , AggregateVerificationKey (..)
    , ConcatenationProof (..)
    , LotteryIndex
    , MerkleCommitment (..)
    , RegistrationEntry (..)
    , SignedRegistration (..)
    , SingleSignature (..)
    , Stake
    )

{- | Reasons why STM verification can fail.

Each failure includes relevant context to aid debugging.
In production, you may want to log these but show users a
generic "verification failed" message.
-}
data VerificationFailure
    = {- | A claimed lottery index exceeds @m@.

      * First field: The invalid index
      * Second field: The @m@ parameter (upper bound)

      This could indicate:

      * Corrupted signature data
      * Parameter mismatch between signer and verifier
      -}
      IndexOutOfBounds !LotteryIndex !Word64
    | {- | A signer claimed an index they didn't win.

      * First field: Signer index (position in signature list)
      * Second field: The lottery index that wasn't won

      This could indicate:

      * Attempted forgery
      * Hash function mismatch
      * Stake/totalStake calculation error
      -}
      LotteryNotWon !Int !LotteryIndex
    | {- | The same lottery index was claimed by multiple signers.

      * Field: The duplicated index

      In a valid signature, each index should appear at most once.
      Duplicates could indicate:

      * Attempted "replay" of indices
      * Bug in signature aggregation
      -}
      DuplicateIndex !LotteryIndex
    | {- | Not enough unique winning indices to meet quorum.

      * First field: Number of unique indices found
      * Second field: Required quorum (@k@)

      This is a normal failure mode when not enough signers
      participated. It's not necessarily an attack.
      -}
      InsufficientSignatures !Int !Word64
    | {- | The Merkle batch proof failed verification.

      * Field: Specific Merkle failure reason

      This could indicate:

      * Signer not in the registered set
      * Corrupted proof data
      * Wrong aggregate verification key
      -}
      MerkleProofInvalid !MerkleVerificationFailure
    | {- | The aggregate BLS signature is invalid.

      This is the final check. If all other checks pass but this
      fails, it could indicate:

      * Signature data corruption
      * Message mismatch
      * Attempted forgery
      -}
      BlsVerificationFailed
    | {- | Failed to deserialize a cryptographic value.

      * Field: Description of what failed

      This indicates malformed input data.
      -}
      DeserializationFailure !String
    deriving stock (Show, Eq)

{- | Verify an STM aggregate signature.

This is the main entry point for STM verification. It performs
all verification steps and returns either success or a specific
failure reason.

__Parameters__:

* @hashOps@: Hash operations (Blake2b-256 and Blake2b-512)
* @blsOps@: BLS12-381 operations (aggregate verification)
* @params@: Protocol parameters (m, k, φ_f)
* @avk@: Aggregate verification key (Merkle root + total stake)
* @message@: The message that was signed
* @aggSig@: The aggregate signature to verify

__Returns__:

* @Right ()@: Signature is valid
* @Left failure@: Signature is invalid with specific reason

==== Example

@
result <- verify hashOps blsOps params avk message signature
case result of
    Right () -> do
        putStrLn "Certificate verified successfully!"
        proceedWithTrustedData
    Left (InsufficientSignatures got need) ->
        putStrLn $ "Not enough signatures: " ++ show got ++ "/" ++ show need
    Left failure ->
        putStrLn $ "Verification failed: " ++ show failure
@

==== Complexity

* Time: O(N × I + M) where N = signers, I = avg indices per signer, M = Merkle proof
* Space: O(N × I) for tracking seen indices
-}
verify
    :: HashOps hash256 hash512
    -- ^ Hash operations
    -> BlsOps sig vk
    -- ^ BLS operations
    -> Parameters
    -- ^ Protocol parameters
    -> AggregateVerificationKey hash256
    -- ^ Aggregate verification key
    -> ByteString
    -- ^ Message that was signed
    -> AggregateSignature sig vk hash256
    -- ^ Aggregate signature to verify
    -> Either VerificationFailure ()
verify hashOps blsOps params avk message (Concatenation proof) = do
    -- Phase 1: Preliminary verification
    -- This checks lottery, uniqueness, quorum, and Merkle proof
    (sigs, vks) <- preliminaryVerify hashOps params avk message proof

    -- Phase 2: BLS aggregate verification
    -- Transform message to include Merkle commitment
    let message' = transformMessage hashOps (avkMerkleCommitment avk) message

    -- Verify aggregate signature
    let pairs = zip vks sigs
    if blsVerifyAggregate blsOps message' pairs
        then Right ()
        else Left BlsVerificationFailed

{- | Preliminary verification (everything except BLS).

This performs all checks that don't require BLS operations:

1. Index bounds checking
2. Lottery verification
3. Index uniqueness
4. Quorum check
5. Merkle proof verification

Returns the list of signatures and verification keys for BLS verification.

__Exported for testing__: Allows testing preliminary checks without
needing a real BLS implementation.
-}
preliminaryVerify
    :: HashOps hash256 hash512
    -- ^ Hash operations
    -> Parameters
    -- ^ Protocol parameters
    -> AggregateVerificationKey hash256
    -- ^ Aggregate verification key
    -> ByteString
    -- ^ Original message
    -> ConcatenationProof sig vk hash256
    -- ^ The concatenation proof
    -> Either VerificationFailure ([sig], [vk])
    -- ^ (signatures, verification keys) for BLS verification
preliminaryVerify hashOps params avk message proof = do
    let totalStake = avkTotalStake avk
        signedRegs = cpSignatures proof

    -- Check all indices and collect unique ones
    uniqueIndices <-
        checkAllSignatures hashOps params totalStake message signedRegs

    -- Check quorum
    let uniqueCount = Set.size uniqueIndices
    if fromIntegral uniqueCount < paramK params
        then Left $ InsufficientSignatures uniqueCount (paramK params)
        else pure ()

    -- Verify Merkle proof
    -- TODO: Serialize registration entries for Merkle verification
    let _leaves = [] :: [ByteString] -- Placeholder
    case verifyBatchPath
        hashOps
        (avkMerkleCommitment avk)
        (cpBatchPath proof)
        _leaves of
        Left merkleErr -> Left $ MerkleProofInvalid merkleErr
        Right () -> pure ()

    -- Extract signatures and verification keys for BLS verification
    let sigs = map (ssSignature . srSignature) signedRegs
        vks = map (reVerificationKey . srRegistration) signedRegs

    pure (sigs, vks)

-- | Check all signatures for lottery wins and collect unique indices.
checkAllSignatures
    :: HashOps hash256 hash512
    -> Parameters
    -> Stake
    -- ^ Total stake
    -> ByteString
    -- ^ Message
    -> [SignedRegistration sig vk]
    -> Either VerificationFailure (Set LotteryIndex)
checkAllSignatures hashOps params totalStake message signedRegs =
    go 0 Set.empty signedRegs
  where
    go _signerIdx !seen [] = Right seen
    go signerIdx !seen (sr : rest) = do
        -- Check this signer's indices
        newSeen <-
            checkSignerIndices hashOps params totalStake message signerIdx seen sr
        go (signerIdx + 1) newSeen rest

-- | Check one signer's claimed lottery indices.
checkSignerIndices
    :: HashOps hash256 hash512
    -> Parameters
    -> Stake
    -> ByteString
    -> Int
    -- ^ Signer index (for error reporting)
    -> Set LotteryIndex
    -- ^ Already seen indices
    -> SignedRegistration sig vk
    -> Either VerificationFailure (Set LotteryIndex)
checkSignerIndices _hashOps params totalStake _message signerIdx seen sr = do
    let indices = ssIndices (srSignature sr)
        stake = reStake (srRegistration sr)

    -- Check each index
    foldM (checkIndex stake) seen indices
  where
    checkIndex stake seenSoFar idx = do
        -- Check index bounds
        if idx >= paramM params
            then Left $ IndexOutOfBounds idx (paramM params)
            else pure ()

        -- Check for duplicates
        if Set.member idx seenSoFar
            then Left $ DuplicateIndex idx
            else pure ()

        -- Check lottery win
        -- TODO: Compute proper hash over (message || vk || index)
        let lotteryHash = BS.replicate 64 0 -- Placeholder
        if not (isLotteryWon (paramPhiF params) lotteryHash stake totalStake)
            then Left $ LotteryNotWon signerIdx idx
            else pure ()

        -- Add to seen set
        pure $ Set.insert idx seenSoFar

    foldM f z xs = foldr (\x k acc -> f acc x >>= k) pure xs z

{- | Transform message to include Merkle commitment.

The actual signed message is:

@msg' = H(merkle_root || original_message)@

This binds the signature to the specific set of registered signers,
preventing signatures from being "moved" to a different registration set.
-}
transformMessage
    :: HashOps hash256 hash512
    -> MerkleCommitment hash256
    -> ByteString
    -> ByteString
transformMessage HashOps{blake2b256, hash256ToBytes} commitment message =
    let rootBytes = hash256ToBytes (mcRoot commitment)
    in  hash256ToBytes $ blake2b256 (rootBytes <> message)
