{- |
Module      : Mithril.STM.Parameters
Description : STM protocol parameters
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

= Overview

This module defines the protocol parameters that govern STM signature
creation and verification. These parameters control the security and
efficiency tradeoffs of the protocol.

= The Three Parameters

== @m@ - Security Parameter (Lottery Space Size)

The parameter @m@ defines the size of the lottery space. Each signer
can potentially win lottery indices from 0 to @m - 1@.

* __Larger @m@__: More possible indices, finer-grained stake representation
* __Smaller @m@__: Fewer indices, coarser but faster

== @k@ - Quorum Parameter (Minimum Winning Indices)

The parameter @k@ is the minimum number of unique winning lottery indices
required for a valid signature.

* __Larger @k@__: More signatures needed, higher security
* __Smaller @k@__: Fewer signatures needed, faster but less secure

__Security guarantee__: An adversary controlling less than the threshold
stake cannot forge a signature (with overwhelming probability).

== @φ_f@ (phi_f) - Base Probability

The parameter @φ_f@ is the base probability used in the lottery.
The actual win probability for a signer with stake proportion @w@ is:

@
φ(w) = 1 - (1 - φ_f)^w
@

* __Larger @φ_f@__: Higher win probability, more signatures expected
* __Smaller @φ_f@__: Lower win probability, fewer signatures

= Parameter Selection

The parameters must satisfy certain constraints for security:

@
k ≤ m × E[winners]
@

where @E[winners]@ is the expected number of winning indices given the
stake distribution.

= Mainnet Parameters

Cardano mainnet uses:

* @m = 20973@
* @k = 2422@
* @φ_f = 0.2@

These were chosen to provide 128-bit security against forgery while
keeping signature sizes reasonable (~30-50 KB with ~200 signers).
-}
module Mithril.STM.Parameters
    ( -- * Parameters Type
      Parameters (..)

      -- * Known Parameter Sets
    , mainnetParameters
    ) where

import Data.Word (Word64)

{- | STM protocol parameters.

These parameters must match between signers and verifiers.
Using mismatched parameters will cause verification to fail.

__Serialization__: 24 bytes total (big-endian)

* @m@: 8 bytes (Word64)
* @k@: 8 bytes (Word64)
* @φ_f@: 8 bytes (IEEE 754 Double)

==== Security Considerations

The parameters determine the security level:

* __Forgery resistance__: Controlled by @k@ and stake distribution
* __Lottery fairness__: Controlled by @φ_f@ and @m@

An attacker with @α@ fraction of stake can forge with probability
roughly @(α/threshold)^k@ where threshold depends on @φ_f@.

==== Example

@
-- Mainnet parameters
params = Parameters
    { paramM = 20973
    , paramK = 2422
    , paramPhiF = 0.2
    }
@
-}
data Parameters = Parameters
    { paramM :: !Word64
    {- ^ Security parameter @m@: upper bound on lottery indices.

    Valid lottery indices are in the range @[0, m-1]@.

    __Constraints__:

    * Must be > 0
    * Typical range: 1000 - 100000
    * Mainnet: 20973
    -}
    , paramK :: !Word64
    {- ^ Quorum parameter @k@: minimum winning indices required.

    A signature is valid only if the total number of unique
    winning lottery indices across all signers is >= @k@.

    __Constraints__:

    * Must be > 0
    * Must be <= @m@
    * Mainnet: 2422
    -}
    , paramPhiF :: !Double
    {- ^ Base probability parameter @φ_f@ (phi_f).

    Used in the lottery win probability formula:

    @
    φ(w) = 1 - (1 - φ_f)^w
    @

    where @w@ is the signer's stake as a fraction of total stake.

    __Constraints__:

    * Must be in range (0, 1)
    * Typical range: 0.1 - 0.5
    * Mainnet: 0.2

    __Interpretation__:

    * @φ_f = 0.2@ means a signer with 100% of stake wins each
    index with probability 0.2
    * A signer with 1% of stake wins with probability
    @1 - 0.8^0.01 ≈ 0.00223@
    -}
    }
    deriving stock (Show, Eq)

{- | Cardano mainnet STM parameters.

These are the production parameters used on Cardano mainnet.
They provide approximately 128-bit security against forgery
assuming the adversary controls less than 1/3 of stake.

@
mainnetParameters = Parameters
    { paramM = 20973
    , paramK = 2422
    , paramPhiF = 0.2
    }
@
-}
mainnetParameters :: Parameters
mainnetParameters =
    Parameters
        { paramM = 20973
        , paramK = 2422
        , paramPhiF = 0.2
        }
