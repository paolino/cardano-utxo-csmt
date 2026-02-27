{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Cardano.Node.Client.E2E.ProviderSpec
Description : E2E tests for the N2C Provider
License     : Apache-2.0
-}
module Cardano.Node.Client.E2E.ProviderSpec
    ( spec
    ) where

import Lens.Micro ((^.))
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldSatisfy
    )

import Cardano.Ledger.Api.PParams (ppMaxTxSizeL)

import Cardano.Node.Client.E2E.Setup
    ( genesisAddr
    , withDevnet
    )
import Cardano.Node.Client.N2C.Provider
    ( mkN2CProvider
    )
import Cardano.Node.Client.Provider
    ( Provider (..)
    )

spec :: Spec
spec =
    around withDevnet'
        $ describe "Provider.N2C"
        $ do
            it
                "queryProtocolParams returns \
                \valid PParams"
                $ \(lsq, _) -> do
                    let provider =
                            mkN2CProvider lsq
                    pp <-
                        queryProtocolParams
                            provider
                    let maxTxSize =
                            pp ^. ppMaxTxSizeL
                    maxTxSize
                        `shouldSatisfy` (> 0)

            it "queryUTxOs returns genesis funds"
                $ \(lsq, _) -> do
                    let provider =
                            mkN2CProvider lsq
                    utxos <-
                        queryUTxOs
                            provider
                            genesisAddr
                    utxos
                        `shouldSatisfy` (not . null)
  where
    withDevnet' action =
        withDevnet $ curry action
