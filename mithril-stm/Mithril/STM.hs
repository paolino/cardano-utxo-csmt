{- |
Module      : Mithril.STM
Description : Mithril Stake-based Threshold Multisignature verification
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module provides native Haskell verification of Mithril STM (Stake-based
Threshold Multisignature) certificates. Mithril is a protocol designed by
IOHK/IOG for creating lightweight certificates that can prove properties
about the Cardano blockchain without requiring a full node.

= What is STM?

STM (Stake-based Threshold Multisignatures) is a cryptographic protocol where:

* A set of signers, each with a stake weight, can collectively sign a message
* The signature is valid if signers representing at least a threshold of
  total stake have signed
* Individual signers participate in a "lottery" based on their stake to
  determine if they can contribute to the signature

= Protocol Parameters

The STM protocol is governed by three parameters:

* @m@ - Security parameter, the upper bound on lottery indices
* @k@ - Quorum parameter, minimum number of winning lottery indices needed
* @φ_f@ (phi_f) - Base probability parameter for the lottery

= Verification Process

To verify an STM aggregate signature:

1. __Lottery Check__: For each signer, verify they won the lottery for their
   claimed indices. The lottery is deterministic based on:
   - The message being signed
   - The signer's verification key
   - The lottery index
   - The signer's stake relative to total stake

2. __Quorum Check__: Verify that the total number of unique winning lottery
   indices meets or exceeds @k@

3. __Merkle Proof__: Verify that all signers are registered in the stake
   distribution commitment (a Merkle tree)

4. __BLS Verification__: Verify the aggregate BLS signature

= Design Philosophy

This library is designed to be __abstract__ over cryptographic primitives.
Instead of hardcoding dependencies on specific BLS or hashing libraries,
it accepts "function records" (also known as "handles") that provide the
necessary operations. This allows:

* Testing with mock implementations
* Swapping backends without changing verification logic
* Avoiding typeclass complexity and orphan instances

= Usage Example

@
import Mithril.STM
import Mithril.STM.Crypto.Hsblst (hsblstOps)
import Mithril.STM.Crypto.Crypton (cryptonHashOps)

-- Verify a certificate
result <- verify cryptonHashOps hsblstOps params avk message aggSig
case result of
    Left failure -> handleError failure
    Right () -> certificateValid
@

= References

* Mithril paper: <https://iohk.io/en/research/library/papers/mithril-stake-based-threshold-multisignatures/>
* Mithril documentation: <https://mithril.network/doc/>
* Rust implementation: <https://github.com/input-output-hk/mithril/tree/main/mithril-stm>
-}
module Mithril.STM
    ( -- * Verification
      -- $verification
      verify
    , VerificationFailure (..)

      -- * Protocol Parameters
      -- $parameters
    , Parameters (..)

      -- * Core Types
      -- $types
    , Stake
    , LotteryIndex
    , RegistrationEntry (..)
    , SingleSignature (..)
    , SignedRegistration (..)
    , ConcatenationProof (..)
    , AggregateSignature (..)
    , AggregateVerificationKey (..)
    , MerkleCommitment (..)
    , MerkleBatchPath (..)

      -- * Crypto Abstractions
      -- $crypto
    , HashOps (..)
    , BlsOps (..)
    ) where

import Mithril.STM.Crypto
    ( BlsOps (..)
    , HashOps (..)
    )
import Mithril.STM.Parameters (Parameters (..))
import Mithril.STM.Types
    ( AggregateSignature (..)
    , AggregateVerificationKey (..)
    , ConcatenationProof (..)
    , LotteryIndex
    , MerkleBatchPath (..)
    , MerkleCommitment (..)
    , RegistrationEntry (..)
    , SignedRegistration (..)
    , SingleSignature (..)
    , Stake
    )
import Mithril.STM.Verify
    ( VerificationFailure (..)
    , verify
    )

{- $verification
The 'verify' function is the main entry point for certificate verification.
It takes crypto operation records, protocol parameters, the verification
key, message, and signature, returning either a failure reason or success.
-}

{- $parameters
Protocol parameters control the security/efficiency tradeoffs of STM.
Mainnet Cardano uses: @k = 2422@, @m = 20973@, @φ_f = 0.2@
-}

{- $types
Core data types representing STM signatures and verification keys.
All types are parameterized over the concrete signature, verification key,
and hash types, allowing different crypto backends.
-}

{- $crypto
Function records providing cryptographic operations. These are passed
explicitly to verification functions rather than using typeclasses.
-}
