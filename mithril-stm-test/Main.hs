{- |
Module      : Main
Description : Test suite entry point for mithril-stm
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0
-}
module Main (main) where

import Test.Hspec

import Mithril.STM.LotterySpec qualified as LotterySpec
import Mithril.STM.MerkleSpec qualified as MerkleSpec
import Mithril.STM.VerifySpec qualified as VerifySpec

main :: IO ()
main = hspec $ do
    describe "Mithril.STM.Lottery" LotterySpec.spec
    describe "Mithril.STM.Merkle" MerkleSpec.spec
    describe "Mithril.STM.Verify" VerifySpec.spec
