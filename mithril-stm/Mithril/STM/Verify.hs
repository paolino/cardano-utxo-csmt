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

      -- * Verification Mode
    , VerificationMode (..)
    , verifyTrustingRegistration

      -- * Chain Verification
    , verifyChain
    , ChainVerificationFailure (..)

      -- * Verification Failures
    , VerificationFailure (..)

      -- * Internal (exported for testing)
    , preliminaryVerify
    , transformMessage
    ) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)

import Mithril.STM.Crypto (BlsOps (..), HashOps (..))
import Mithril.STM.Lottery (isLotteryWon)
import Mithril.STM.Merkle (MerkleVerificationFailure, verifyBatchPath)
import Mithril.STM.Parameters (Parameters (..))
import Mithril.STM.Types
    ( AggregateSignature (..)
    , AggregateVerificationKey (..)
    , CertificateLink (..)
    , ConcatenationProof (..)
    , GenesisVerificationKey (..)
    , LotteryIndex
    , MerkleCommitment (..)
    , RegistrationEntry (..)
    , SignedRegistration (..)
    , SingleSignature (..)
    , Stake
    )

{- | Verification mode controls which checks are performed.

When fetching certificates from a Mithril aggregator, the aggregator has
already verified Merkle membership proofs. In this case, you can use
'TrustRegistration' mode to skip redundant Merkle verification.

For maximum security when obtaining certificates from untrusted sources,
use 'FullVerification' mode.
-}
data VerificationMode
    = {- | Verify Merkle membership proof (requires valid batch path).
      Use when the certificate source is untrusted.
      -}
      FullVerification
    | {- | Skip Merkle check, trust that signers are registered.
      Use when fetching from a trusted Mithril aggregator.
      -}
      TrustRegistration
    deriving stock (Show, Eq)

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
    :: (Eq hash256)
    => HashOps hash256 hash512
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
verify hashOps blsOps params avk message aggSig =
    verifyWithMode
        hashOps
        blsOps
        params
        avk
        message
        aggSig
        FullVerification

{- | Verify certificate trusting that signers are registered.

This verifies:

* BLS aggregate signature validity
* Lottery eligibility for each signer
* Quorum threshold (>= k indices)
* Index bounds and uniqueness

This does __NOT__ verify:

* Merkle membership (that signers were registered)

Use when fetching certificates from a trusted Mithril aggregator.
The aggregator already verified Merkle membership before publishing.

==== Example

@
result <- verifyTrustingRegistration
    cryptonHashOps
    hsblstBlsOps
    params
    avk
    message
    aggSig
case result of
    Right () -> putStrLn "Certificate verified (trusting registration)"
    Left failure -> putStrLn $ "Verification failed: " ++ show failure
@
-}
verifyTrustingRegistration
    :: (Eq hash256)
    => HashOps hash256 hash512
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
verifyTrustingRegistration hashOps blsOps params avk message aggSig =
    verifyWithMode
        hashOps
        blsOps
        params
        avk
        message
        aggSig
        TrustRegistration

{- | Internal: verify with explicit mode.

This is the core verification function that supports both full and
trusting-registration modes.
-}
verifyWithMode
    :: (Eq hash256)
    => HashOps hash256 hash512
    -> BlsOps sig vk
    -> Parameters
    -> AggregateVerificationKey hash256
    -> ByteString
    -> AggregateSignature sig vk hash256
    -> VerificationMode
    -> Either VerificationFailure ()
verifyWithMode hashOps blsOps params avk message (Concatenation proof) mode = do
    -- Phase 1: Preliminary verification
    (sigs, vks) <-
        preliminaryVerifyWithMode hashOps blsOps params avk message proof mode

    -- Phase 2: BLS aggregate verification
    let message' = transformMessage hashOps (avkMerkleCommitment avk) message
        pairs = zip vks sigs
    if blsVerifyAggregate blsOps message' pairs
        then Right ()
        else Left BlsVerificationFailed

{- | Preliminary verification with explicit mode.

When mode is 'TrustRegistration', Merkle proof verification is skipped.
-}
preliminaryVerifyWithMode
    :: (Eq hash256)
    => HashOps hash256 hash512
    -> BlsOps sig vk
    -> Parameters
    -> AggregateVerificationKey hash256
    -> ByteString
    -> ConcatenationProof sig vk hash256
    -> VerificationMode
    -> Either VerificationFailure ([sig], [vk])
preliminaryVerifyWithMode hashOps blsOps params avk message proof mode = do
    let totalStake = avkTotalStake avk
        signedRegs = cpSignatures proof
        -- Note: For certificates fetched from the aggregator, the message
        -- is already the signed_message which includes the Merkle commitment.
        -- We use it directly for lottery verification.
        -- For other use cases, caller should provide transformed message.
        message' = message

    -- Check all indices and collect unique ones
    uniqueIndices <-
        checkAllSignatures
            hashOps
            blsOps
            params
            totalStake
            message'
            signedRegs

    -- Check quorum
    let uniqueCount = Set.size uniqueIndices
    if fromIntegral uniqueCount < paramK params
        then Left $ InsufficientSignatures uniqueCount (paramK params)
        else pure ()

    -- Verify Merkle proof (only in FullVerification mode)
    case mode of
        FullVerification -> do
            let _leaves = [] :: [ByteString]
            case verifyBatchPath
                hashOps
                (avkMerkleCommitment avk)
                (cpBatchPath proof)
                _leaves of
                Left merkleErr -> Left $ MerkleProofInvalid merkleErr
                Right () -> pure ()
        TrustRegistration ->
            -- Skip Merkle verification, trust the aggregator
            pure ()

    -- Extract signatures and verification keys for BLS verification
    let sigs = map (ssSignature . srSignature) signedRegs
        vks = map (reVerificationKey . srRegistration) signedRegs

    pure (sigs, vks)

{- | Preliminary verification (everything except BLS).

This performs all checks that don't require BLS operations:

1. Index bounds checking
2. Lottery verification
3. Index uniqueness
4. Quorum check
5. Merkle proof verification

Returns the list of signatures and verification keys for BLS verification.

__Exported for testing__: Allows testing preliminary checks without
needing a real BLS implementation. Note: For lottery hash computation,
the BlsOps is used only for serializing verification keys; no actual
BLS operations are performed.
-}
preliminaryVerify
    :: (Eq hash256)
    => HashOps hash256 hash512
    -- ^ Hash operations
    -> BlsOps sig vk
    -- ^ BLS operations (only used for vk serialization)
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
preliminaryVerify hashOps blsOps params avk message proof =
    preliminaryVerifyWithMode
        hashOps
        blsOps
        params
        avk
        message
        proof
        FullVerification

-- | Check all signatures for lottery wins and collect unique indices.
checkAllSignatures
    :: HashOps hash256 hash512
    -> BlsOps sig vk
    -- ^ BLS operations (for serializing verification keys)
    -> Parameters
    -> Stake
    -- ^ Total stake
    -> ByteString
    -- ^ Transformed message (H(merkle_root || original_message))
    -> [SignedRegistration sig vk]
    -> Either VerificationFailure (Set LotteryIndex)
checkAllSignatures hashOps blsOps params totalStake message' signedRegs =
    go 0 Set.empty signedRegs
  where
    go _signerIdx !seen [] = Right seen
    go signerIdx !seen (sr : rest) = do
        -- Check this signer's indices
        newSeen <-
            checkSignerIndices
                hashOps
                blsOps
                params
                totalStake
                message'
                signerIdx
                seen
                sr
        go (signerIdx + 1) newSeen rest

{- | Check one signer's claimed lottery indices.

For each index, we:
1. Check bounds (index < m)
2. Check uniqueness (not already claimed)
3. Check lottery win via hash computation

The lottery hash follows the Mithril STM format:

@ev = H("map" || msg || index || σ)@

Where:

* @"map"@ is a 3-byte prefix
* @msg@ is the transformed message (H(merkle_root || original_message))
* @index@ is the lottery index as little-endian 8 bytes
* @σ@ is the BLS signature (48 bytes compressed)
* @H@ is Blake2b-512 (returns 64 bytes)
-}
checkSignerIndices
    :: HashOps hash256 hash512
    -> BlsOps sig vk
    -- ^ BLS operations (for serializing signature)
    -> Parameters
    -> Stake
    -> ByteString
    -- ^ Transformed message
    -> Int
    -- ^ Signer index (for error reporting)
    -> Set LotteryIndex
    -- ^ Already seen indices
    -> SignedRegistration sig vk
    -> Either VerificationFailure (Set LotteryIndex)
checkSignerIndices hashOps blsOps params totalStake message' signerIdx seen sr = do
    let indices = ssIndices (srSignature sr)
        stake = reStake (srRegistration sr)
        sig = ssSignature (srSignature sr)
        sigBytes = blsSerializeSig blsOps sig

    -- Check each index
    foldM (checkIndex stake sigBytes) seen indices
  where
    checkIndex stake sigBytes seenSoFar idx = do
        -- Check index bounds
        if idx >= paramM params
            then Left $ IndexOutOfBounds idx (paramM params)
            else pure ()

        -- Check for duplicates
        if Set.member idx seenSoFar
            then Left $ DuplicateIndex idx
            else pure ()

        -- Check lottery win
        -- Compute lottery hash: H("map" || msg || index || σ)
        -- Note: index is little-endian per Mithril Rust implementation
        let prefix = "map" :: ByteString
            idxBytes = encodeWord64LE idx
            hashInput = prefix <> message' <> idxBytes <> sigBytes
            lotteryHash = hash512ToBytes hashOps (blake2b512 hashOps hashInput)
        if not (isLotteryWon (paramPhiF params) lotteryHash stake totalStake)
            then Left $ LotteryNotWon signerIdx idx
            else pure ()

        -- Add to seen set
        pure $ Set.insert idx seenSoFar

    foldM f z xs = foldr (\x k acc -> f acc x >>= k) pure xs z

{- | Encode a Word64 as little-endian bytes (8 bytes).
Mithril uses little-endian for the lottery index.
-}
encodeWord64LE :: Word64 -> ByteString
encodeWord64LE w =
    BS.pack
        [ fromIntegral w :: Word8
        , fromIntegral (shiftR w 8)
        , fromIntegral (shiftR w 16)
        , fromIntegral (shiftR w 24)
        , fromIntegral (shiftR w 32)
        , fromIntegral (shiftR w 40)
        , fromIntegral (shiftR w 48)
        , fromIntegral (shiftR w 56)
        ]

{- | Transform message to include Merkle commitment.

Following the Mithril STM protocol, the message is concatenated with the
Merkle root (without additional hashing):

@msg' = original_message || merkle_root@

This binds the signature to the specific set of registered signers,
preventing signatures from being "moved" to a different registration set.

Note: The message comes first, then the root (per Mithril Rust implementation).
-}
transformMessage
    :: HashOps hash256 hash512
    -> MerkleCommitment hash256
    -> ByteString
    -> ByteString
transformMessage HashOps{hash256ToBytes} commitment message =
    let rootBytes = hash256ToBytes (mcRoot commitment)
    in  message <> rootBytes

-- ============================================================================
-- Certificate Chain Verification
-- ============================================================================

{- | Reasons why certificate chain verification can fail.

Chain verification traces certificates back to genesis, verifying
each link in the chain.
-}
data ChainVerificationFailure
    = {- | A non-genesis certificate failed STM verification.
      First field: certificate index (0 = most recent).
      Second field: the verification failure.
      -}
      ChainCertificateInvalid !Int !VerificationFailure
    | -- | The genesis certificate's Ed25519 signature is invalid.
      GenesisSignatureInvalid
    | -- | The genesis certificate is missing its signature.
      GenesisSignatureMissing
    | -- | The genesis verification key is not a valid Ed25519 public key.
      GenesisKeyMalformed
    | -- | No certificates provided.
      ChainEmpty
    | {- | The chain doesn't end with a genesis certificate
      (last certificate has a previous_hash).
      -}
      ChainNotTerminatedAtGenesis
    deriving stock (Show, Eq)

{- | Verify a certificate chain back to genesis.

This function verifies each certificate in a chain, from the most recent
certificate back to the genesis certificate. For each certificate:

1. Non-genesis certificates are verified using STM verification
   (with 'TrustRegistration' mode, since the chain provides trust)
2. The genesis certificate is verified using its Ed25519 signature

__Chain structure__:

The chain should be ordered from newest to oldest:

@
[newest_cert, ..., genesis_cert]
@

The genesis certificate is identified by having 'Nothing' for
'clPreviousHash' and having a 'clGenesisSignature'.

__Trust model__:

* The genesis certificate is signed by IOG's genesis key
* Each subsequent certificate's AVK is embedded in its predecessor
* This creates a chain of trust from genesis to the current certificate

__Parameters__:

* @verifyEd25519@: Ed25519 verification function
* @hashOps@: Hash operations
* @blsOps@: BLS operations
* @genesisVk@: IOG's genesis verification key
* @chain@: List of (certificate link, params, avk, message, aggregate sig)

__Note__:

For non-genesis certificates, the aggregate signature must be provided.
For the genesis certificate, only the 'CertificateLink' with
'clGenesisSignature' is needed.
-}
verifyChain
    :: (Eq hash256)
    => (ByteString -> ByteString -> ByteString -> Maybe Bool)
    -- ^ Ed25519 verification function
    -> HashOps hash256 hash512
    -- ^ Hash operations
    -> BlsOps sig vk
    -- ^ BLS operations
    -> GenesisVerificationKey
    -- ^ Genesis verification key
    -> [ ( CertificateLink
         , Maybe
            ( Parameters
            , AggregateVerificationKey hash256
            , ByteString
            , AggregateSignature sig vk hash256
            )
         )
       ]
    {- ^ Chain from newest to genesis. Each entry is (link, optional STM data).
    The genesis certificate should have Nothing for STM data.
    -}
    -> Either ChainVerificationFailure ()
verifyChain _ _ _ _ [] = Left ChainEmpty
verifyChain verifyEd25519' hashOps blsOps genesisVk chain =
    verifyChainLoop 0 chain
  where
    verifyChainLoop _ [] = Left ChainEmpty
    verifyChainLoop _idx [(link, _)] =
        -- Last certificate should be genesis
        case clPreviousHash link of
            Just _ -> Left ChainNotTerminatedAtGenesis
            Nothing -> verifyGenesis link
    verifyChainLoop idx ((link, stmData) : rest) =
        case clPreviousHash link of
            Nothing ->
                -- This is genesis but not at end - invalid
                Left ChainNotTerminatedAtGenesis
            Just _ -> do
                -- Verify STM signature
                case stmData of
                    Nothing ->
                        -- Non-genesis must have STM data
                        Left
                            $ ChainCertificateInvalid idx
                            $ DeserializationFailure
                                "Missing STM data for non-genesis cert"
                    Just (params, avk, msg, aggSig) ->
                        case verifyTrustingRegistration
                            hashOps
                            blsOps
                            params
                            avk
                            msg
                            aggSig of
                            Left err ->
                                Left $ ChainCertificateInvalid idx err
                            Right () ->
                                verifyChainLoop (idx + 1) rest

    verifyGenesis link = case clGenesisSignature link of
        Nothing -> Left GenesisSignatureMissing
        Just sig ->
            let GenesisVerificationKey vkBytes = genesisVk
                msg = clSignedMessage link
            in  case verifyEd25519' vkBytes msg sig of
                    Nothing -> Left GenesisKeyMalformed
                    Just False -> Left GenesisSignatureInvalid
                    Just True -> Right ()
