{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Cardano.Node.Client.E2E.UTxOStateSpec
Description : E2E tests verifying UTxO state transitions
License     : Apache-2.0

Tests that the UTxO KV set actually changes after
transfers: spent UTxOs vanish, new UTxOs appear,
and chained transfers propagate correctly.
-}
module Cardano.Node.Client.E2E.UTxOStateSpec
    ( spec
    ) where

import Data.List (find)
import Lens.Micro ((^.))
import Test.Hspec
    ( Spec
    , SpecWith
    , around
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

import Cardano.Ledger.Api.Tx.Out (coinTxOutL)
import Cardano.Ledger.Coin (Coin (..))

import Cardano.Node.Client.E2E.Setup
    ( buildTransfer
    , enterpriseAddr
    , genesisAddr
    , genesisSignKey
    , keyHashFromSignKey
    , mkSignKey
    , waitForUTxOChange
    , withDevnet
    )
import Cardano.Node.Client.N2C.Provider
    ( mkN2CProvider
    )
import Cardano.Node.Client.N2C.Submitter
    ( mkN2CSubmitter
    )
import Cardano.Node.Client.N2C.Types
    ( LSQChannel
    , LTxSChannel
    )
import Cardano.Node.Client.Provider
    ( Provider (..)
    )
import Cardano.Node.Client.Submitter
    ( SubmitResult (..)
    , Submitter (..)
    )

spec :: Spec
spec =
    around withDevnet'
        $ describe "UTxO state transitions"
        $ do
            theDragonsHoard
            theVanishingCoin
            theRelayRace
  where
    withDevnet' action =
        withDevnet $ curry action

-- ── The Dragon's Hoard ──────────────────────

{- | Genesis has a massive UTxO. Send 10 ADA to
a fresh address ("the thief"). Verify:

- Genesis UTxO set changed (the original fat
  UTxO gone, a change UTxO appeared)
- Thief's address now has exactly one UTxO
  with >= 10 ADA
-}
theDragonsHoard :: SpecWith (LSQChannel, LTxSChannel)
theDragonsHoard =
    it
        "The Dragon's Hoard — theft creates \
        \new UTxO at thief address"
        $ \(lsq, ltxs) -> do
            let provider = mkN2CProvider lsq
                submitter = mkN2CSubmitter ltxs
                thiefSk =
                    mkSignKey
                        "e2e-thief-key-seed-00000001"
                thiefAddr =
                    enterpriseAddr
                        (keyHashFromSignKey thiefSk)
                tenAda = Coin 10_000_000
            pp <- queryProtocolParams provider
            genBefore <-
                queryUTxOs provider genesisAddr
            genBefore
                `shouldSatisfy` (not . null)
            let signed =
                    buildTransfer
                        pp
                        genBefore
                        genesisSignKey
                        genesisAddr
                        thiefAddr
                        tenAda
            result <- submitTx submitter signed
            result `shouldSatisfy` isSubmitted
            genAfter <-
                waitForUTxOChange
                    provider
                    genesisAddr
                    genBefore
            -- Genesis set must have changed
            map fst genAfter
                `shouldSatisfy` (/= map fst genBefore)
            -- Thief now has exactly one UTxO
            thiefUtxos <-
                queryUTxOs provider thiefAddr
            length thiefUtxos `shouldBe` 1
            -- That UTxO holds >= 10 ADA
            case thiefUtxos of
                [(_, out)] ->
                    (out ^. coinTxOutL)
                        `shouldSatisfy` (>= tenAda)
                _ ->
                    error "unexpected thief UTxO count"

-- ── The Vanishing Coin ──────────────────────

{- | Record a specific 'TxIn' from genesis.
Submit a tx consuming it. Poll until that 'TxIn'
is gone from 'queryUTxOs'. Proves the CSMT would
process a @Spend@ for this key.
-}
theVanishingCoin :: SpecWith (LSQChannel, LTxSChannel)
theVanishingCoin =
    it
        "The Vanishing Coin — spent TxIn \
        \disappears from UTxO set"
        $ \(lsq, ltxs) -> do
            let provider = mkN2CProvider lsq
                submitter = mkN2CSubmitter ltxs
                nobodySk =
                    mkSignKey
                        "e2e-nobody-key-seed-0000001"
                nobodyAddr =
                    enterpriseAddr
                        (keyHashFromSignKey nobodySk)
                oneAda = Coin 1_000_000
            pp <- queryProtocolParams provider
            genBefore <-
                queryUTxOs provider genesisAddr
            case genBefore of
                [] -> error "no genesis UTxOs"
                ((targetTxIn, _) : _) -> do
                    let signed =
                            buildTransfer
                                pp
                                genBefore
                                genesisSignKey
                                genesisAddr
                                nobodyAddr
                                oneAda
                    result <-
                        submitTx submitter signed
                    result
                        `shouldSatisfy` isSubmitted
                    genAfter <-
                        waitForUTxOChange
                            provider
                            genesisAddr
                            genBefore
                    -- The specific TxIn must be gone
                    find
                        ((== targetTxIn) . fst)
                        genAfter
                        `shouldBe` Nothing

-- ── The Relay Race ──────────────────────────

{- | Chain: genesis → Alice → Bob.

1. Send 20 ADA from genesis to Alice
2. Wait for Alice to have funds
3. Send 10 ADA from Alice to Bob
4. Verify Bob has funds, Alice has change,
   genesis UTxO set changed twice
-}
theRelayRace :: SpecWith (LSQChannel, LTxSChannel)
theRelayRace =
    it
        "The Relay Race — chained transfers \
        \propagate correctly"
        $ \(lsq, ltxs) -> do
            let provider = mkN2CProvider lsq
                submitter = mkN2CSubmitter ltxs
                aliceSk =
                    mkSignKey
                        "e2e-alice-key-seed-0000001"
                aliceAddr =
                    enterpriseAddr
                        (keyHashFromSignKey aliceSk)
                bobSk =
                    mkSignKey
                        "e2e-bobbb-key-seed-0000001"
                bobAddr =
                    enterpriseAddr
                        (keyHashFromSignKey bobSk)
                twentyAda = Coin 20_000_000
                tenAda = Coin 10_000_000
            pp <- queryProtocolParams provider
            -- Step 1: genesis → Alice (20 ADA)
            genBefore <-
                queryUTxOs provider genesisAddr
            let tx1 =
                    buildTransfer
                        pp
                        genBefore
                        genesisSignKey
                        genesisAddr
                        aliceAddr
                        twentyAda
            r1 <- submitTx submitter tx1
            r1 `shouldSatisfy` isSubmitted
            -- Wait for genesis to change
            _genMid <-
                waitForUTxOChange
                    provider
                    genesisAddr
                    genBefore
            -- Step 2: wait for Alice to have funds
            aliceUtxos <-
                waitForUTxOChange
                    provider
                    aliceAddr
                    []
            aliceUtxos
                `shouldSatisfy` (not . null)
            -- Step 3: Alice → Bob (10 ADA)
            let tx2 =
                    buildTransfer
                        pp
                        aliceUtxos
                        aliceSk
                        aliceAddr
                        bobAddr
                        tenAda
            r2 <- submitTx submitter tx2
            r2 `shouldSatisfy` isSubmitted
            -- Wait for Bob to receive funds
            bobUtxos <-
                waitForUTxOChange
                    provider
                    bobAddr
                    []
            bobUtxos `shouldSatisfy` (not . null)
            -- Bob has exactly one UTxO >= 10 ADA
            length bobUtxos `shouldBe` 1
            case bobUtxos of
                [(_, out)] ->
                    (out ^. coinTxOutL)
                        `shouldSatisfy` (>= tenAda)
                _ ->
                    error "unexpected Bob UTxO count"
            -- Alice has change (spent 10 + fee
            -- from 20, remainder left)
            aliceAfter <-
                queryUTxOs provider aliceAddr
            aliceAfter
                `shouldSatisfy` (not . null)
            -- Alice's UTxOs changed from before
            map fst aliceAfter
                `shouldSatisfy` (/= map fst aliceUtxos)

-- ── Helpers ─────────────────────────────────

isSubmitted :: SubmitResult -> Bool
isSubmitted (Submitted _) = True
isSubmitted _ = False
