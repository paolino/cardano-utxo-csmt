{- |
Module      : Mithril.STM.LotterySpec
Description : Tests for lottery eligibility check
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

Tests for the lottery mechanism that determines signer eligibility.
-}
module Mithril.STM.LotterySpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Test.Hspec
import Test.QuickCheck hiding (total)

import Mithril.STM.Lottery
    ( hashToUnitInterval
    , isLotteryWon
    , phi
    )

spec :: Spec
spec = do
    describe "phi (probability function)" $ do
        it "phi(0) = 0 for any phi_f" $ do
            phi 0.2 0 `shouldBe` 0
            phi 0.5 0 `shouldBe` 0
            phi 0.9 0 `shouldBe` 0

        it "phi(1) = phi_f (100% stake = base probability)" $ do
            -- phi(1) = 1 - (1 - phi_f)^1 = phi_f
            phi 0.2 1.0 `shouldBe` 0.2
            phi 0.5 1.0 `shouldBe` 0.5

        it "phi is monotonically increasing"
            $ property
            $ \(phiF :: Double) ->
                let phiF' = abs phiF `mod'` 0.99 + 0.01 -- Keep in (0.01, 1)
                in  phi phiF' 0.3 < phi phiF' 0.7

        it "phi is concave (diminishing returns)"
            $ property
            $
            -- phi(w1 + w2) <= phi(w1) + phi(w2)
            -- This means splitting stake doesn't help an attacker
            \(SmallPositive w1) (SmallPositive w2) ->
                let phiF = 0.2
                    total = min 1.0 (w1 + w2)
                in  phi phiF total <= phi phiF w1 + phi phiF w2 + 1e-10

        it "matches expected values for phi_f = 0.2" $ do
            -- These are approximate values, allowing small floating point error
            phi 0.2 0.01 `shouldSatisfy` approxEq 0.00223 0.001
            phi 0.2 0.1 `shouldSatisfy` approxEq 0.0221 0.001
            phi 0.2 0.5 `shouldSatisfy` approxEq 0.1056 0.01

    describe "hashToUnitInterval" $ do
        it "returns 0 for all-zero hash" $ do
            let zeroHash = BS.replicate 64 0
            hashToUnitInterval zeroHash `shouldBe` 0.0

        it "returns value close to 1 for all-0xFF hash" $ do
            -- Note: Due to Double precision limits, (2^64-1)/2^64 rounds to 1.0
            let maxHash = BS.replicate 64 0xFF
            hashToUnitInterval maxHash `shouldSatisfy` (> 0.99)
            hashToUnitInterval maxHash `shouldSatisfy` (<= 1.0)

        it "returns value in [0, 1) for any hash"
            $ property
            $ \(HashBytes bs) ->
                let v = hashToUnitInterval bs
                in  v >= 0 && v < 1

        it "is deterministic"
            $ property
            $ \(HashBytes bs) ->
                hashToUnitInterval bs == hashToUnitInterval bs

    describe "isLotteryWon" $ do
        it "zero stake never wins" $ do
            let hash = BS.replicate 64 0x80 -- Middle value
            isLotteryWon 0.2 hash 0 1000000 `shouldBe` False

        it "returns False for zero total stake (edge case)" $ do
            let hash = BS.replicate 64 0
            isLotteryWon 0.2 hash 1000 0 `shouldBe` False

        it "always wins when phi_f >= 1" $ do
            let hash = BS.replicate 64 0xFF -- Worst case hash
            isLotteryWon 1.0 hash 1 1000000 `shouldBe` True

        it "higher stake increases win probability"
            $ property
            $
            -- With fixed hash, higher stake should win at least as often
            \(HashBytes hash) ->
                let lowStake = 1000
                    highStake = 100000
                    totalStake = 1000000
                    lowWins = isLotteryWon 0.2 hash lowStake totalStake
                    highWins = isLotteryWon 0.2 hash highStake totalStake
                in  not lowWins || highWins -- If low wins, high must also win
        it "100% stake holder wins for low hash values" $ do
            -- With hash = 0, the normalized value p = 0, which is < phi(1) = 0.2
            let zeroHash = BS.replicate 64 0
            isLotteryWon 0.2 zeroHash 1000000 1000000 `shouldBe` True

        it "100% stake holder loses for high hash values" $ do
            -- With hash = 0xFF..., p ≈ 1, which is > phi(1) = 0.2
            let maxHash = BS.replicate 64 0xFF
            isLotteryWon 0.2 maxHash 1000000 1000000 `shouldBe` False

-- ============================================================================
-- Test Helpers
-- ============================================================================

-- | Approximate equality for floating point
approxEq :: Double -> Double -> Double -> Bool
approxEq expected tolerance actual =
    abs (actual - expected) < tolerance

-- | Modulo for Double (helper for property tests)
mod' :: Double -> Double -> Double
mod' x y = x - y * fromIntegral (floor (x / y) :: Int)

-- | Newtype for generating small positive doubles
newtype SmallPositive = SmallPositive Double
    deriving (Show)

instance Arbitrary SmallPositive where
    arbitrary = SmallPositive . (/ 100) . abs <$> arbitrary

-- | Newtype for generating 64-byte hash values
newtype HashBytes = HashBytes ByteString
    deriving (Show)

instance Arbitrary HashBytes where
    arbitrary = HashBytes . BS.pack <$> vectorOf 64 arbitrary
