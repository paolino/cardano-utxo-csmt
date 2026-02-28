module Main (main) where

import Cardano.UTxOCSMT.E2E.GenesisChainSyncSpec qualified as GenesisChainSyncSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec GenesisChainSyncSpec.spec
