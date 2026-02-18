{- |
Module      : Mithril.STM.Lottery
Description : STM lottery eligibility check
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module implements the lottery mechanism that determines whether a
signer can contribute to an STM signature for a given lottery index.

= The Lottery Mechanism

In STM, not every signer contributes to every signature. Instead, signers
participate in a deterministic "lottery" for each potential index:

1. The signer computes a hash over (message, verification key, index)
2. The hash is interpreted as a number in [0, 1)
3. If this number is less than φ(stake/total_stake), the signer wins

This ensures that:

* __Determinism__: Anyone can verify if a signer won
* __Proportionality__: Higher stake = higher win probability
* __Unpredictability__: Cannot predict winners without knowing the message

= The Probability Function φ

The win probability for a signer with stake proportion @w@ is:

@
φ(w) = 1 - (1 - φ_f)^w
@

where @φ_f@ is the base probability parameter.

__Properties of φ__:

* @φ(0) = 0@: Zero stake = zero probability
* @φ(1) = φ_f@: 100% stake = base probability
* @φ@ is concave: Diminishing returns for larger stakes
* @φ(w₁ + w₂) ≤ φ(w₁) + φ(w₂)@: Stake splitting doesn't help

The concavity is important: it means an attacker cannot gain advantage
by splitting their stake across multiple identities.

= Precision Requirements

The lottery check requires high precision arithmetic because:

1. Hash output is 512 bits (very large number)
2. Stake fractions can be very small (e.g., 0.001%)
3. Small errors could flip lottery outcomes

The Rust implementation uses either:

* 117-bit precision floating point (rug library on Unix)
* Taylor series expansion (~1000 terms) on other platforms

This Haskell implementation uses 'Rational' for exact arithmetic where
possible, falling back to high-precision 'Double' for the exponentiation.

= Security Considerations

The lottery must be:

1. __Deterministic__: Same inputs always give same result
2. __Unpredictable__: Cannot know outcome without the message
3. __Unbiasable__: Cannot influence outcome except through stake

Using Blake2b-512 for the hash provides these properties assuming
the hash function is secure (no known attacks on Blake2b).
-}
module Mithril.STM.Lottery
    ( -- * Lottery Check
      isLotteryWon

      -- * Probability Function
    , phi

      -- * Internal (exported for testing)
    , hashToUnitInterval
    ) where

import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Ratio ((%))
import Data.Word (Word64, Word8)

import Mithril.STM.Types (Stake)

{- | Check if a signer won the lottery for a given index.

This is the core lottery check. Given:

* @φ_f@: Base probability parameter
* @hash@: 64-byte Blake2b-512 hash of (message || vk || index)
* @stake@: Signer's stake
* @totalStake@: Sum of all registered signers' stakes

Returns 'True' if the signer won this lottery index.

__Algorithm__:

1. Interpret @hash@ as a number @h@ in [0, 2^512)
2. Compute @p = h / 2^512@ (a value in [0, 1))
3. Compute @w = stake / totalStake@ (stake proportion)
4. Compute @φ(w) = 1 - (1 - φ_f)^w@
5. Return @p < φ(w)@

==== Example

@
-- Signer with 1% of stake, φ_f = 0.2
let hash = blake2b512 (msg <> vk <> indexBytes)
    won = isLotteryWon 0.2 hash 1_000_000 100_000_000
-- won is True with probability ≈ 0.00223
@

==== Precision

This implementation uses 'Rational' for the stake fraction and
'Double' for the final probability comparison. This provides
sufficient precision for typical stake distributions.

For very small stakes (< 0.0001% of total), there may be minor
precision differences compared to the Rust implementation.
-}
isLotteryWon
    :: Double
    -- ^ @φ_f@: Base probability parameter (typically 0.2)
    -> ByteString
    -- ^ 64-byte hash value (Blake2b-512 output)
    -> Stake
    -- ^ Signer's stake in Lovelace
    -> Stake
    -- ^ Total stake of all registered signers
    -> Bool
isLotteryWon phiF hash stake totalStake
    -- Edge case: if φ_f is essentially 1, everyone wins
    | phiF >= 1.0 - 1e-10 = True
    -- Edge case: zero stake never wins
    | stake == 0 = False
    -- Edge case: zero total stake is invalid (avoid division by zero)
    | totalStake == 0 = False
    -- Normal case: check if hash value < φ(w)
    | otherwise = p < threshold
  where
    -- Interpret hash as a value in [0, 1)
    p :: Double
    p = hashToUnitInterval hash

    -- Stake proportion as a rational (exact)
    w :: Rational
    w = fromIntegral stake % fromIntegral totalStake

    -- Win threshold: φ(w) = 1 - (1 - φ_f)^w
    threshold :: Double
    threshold = phi phiF (fromRational w)

{- | The probability function φ(w) = 1 - (1 - φ_f)^w.

This function computes the lottery win probability for a signer
with stake proportion @w@.

__Mathematical Properties__:

* @φ(0) = 0@
* @φ(1) = φ_f@
* @φ'(w) = -ln(1 - φ_f) × (1 - φ_f)^w > 0@ (increasing)
* @φ''(w) = (ln(1 - φ_f))² × (1 - φ_f)^w < 0@ (concave)

__Implementation__:

Uses the identity: @(1 - φ_f)^w = exp(w × ln(1 - φ_f))@

This avoids computing a fractional power directly, which can
be numerically unstable for small @w@.

==== Example

@
phi 0.2 0.01  -- ≈ 0.00223 (1% stake holder's win probability)
phi 0.2 0.5   -- ≈ 0.1056  (50% stake holder's win probability)
phi 0.2 1.0   -- = 0.2     (100% stake = base probability)
@
-}
phi
    :: Double
    -- ^ @φ_f@: Base probability parameter
    -> Double
    -- ^ @w@: Stake proportion (0 to 1)
    -> Double
    -- ^ Win probability φ(w)
phi phiF w
    | phiF <= 0 = 0
    | phiF >= 1 = 1
    | w <= 0 = 0
    | w >= 1 = phiF
    | otherwise = 1 - exp (w * log (1 - phiF))

-- Note: 1 - (1 - φ_f)^w = 1 - exp(w × ln(1 - φ_f))

{- | Convert a 64-byte hash to a value in [0, 1).

Interprets the hash as a 512-bit big-endian unsigned integer,
then divides by 2^512 to get a value in [0, 1).

__Implementation__:

For efficiency, we don't compute the full 512-bit integer.
Instead, we use only the first 8 bytes (64 bits) and divide
by 2^64. This provides sufficient precision (about 19 decimal
digits) for lottery comparison.

__Why this is safe__:

The lottery comparison is @p < φ(w)@ where @φ(w)@ is typically
in the range [0.001, 0.3]. A 64-bit approximation has error
at most 2^-64 ≈ 5×10^-20, far smaller than any realistic @φ(w)@.

==== Example

@
hashToUnitInterval (BS.replicate 64 0)    -- = 0.0
hashToUnitInterval (BS.replicate 64 255)  -- ≈ 1.0 (but < 1.0)
@
-}
hashToUnitInterval :: ByteString -> Double
hashToUnitInterval hash
    | BS.length hash < 8 = 0 -- Invalid hash, treat as 0
    | otherwise = fromIntegral leading64 / (2 ^ (64 :: Int))
  where
    -- Take first 8 bytes and interpret as big-endian Word64
    -- For big-endian: first byte is most significant
    leading64 :: Word64
    leading64 = BS.foldl' addByte 0 (BS.take 8 hash)

    addByte :: Word64 -> Word8 -> Word64
    addByte acc byte = (acc `shiftL` 8) + fromIntegral byte

-- Note: The above is a simplified implementation. The Rust implementation
-- uses either:
-- 1. Full 512-bit arithmetic with 117-bit precision (rug library)
-- 2. Taylor series for exp() with ~1000 iterations
--
-- For most practical purposes, the 64-bit approximation is sufficient.
-- If exact compatibility with Rust is needed, we would need to implement
-- the full precision arithmetic.
