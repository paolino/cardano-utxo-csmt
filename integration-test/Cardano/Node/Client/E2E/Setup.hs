{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Cardano.Node.Client.E2E.Setup
Description : E2E test helpers for devnet
License     : Apache-2.0
-}
module Cardano.Node.Client.E2E.Setup
    ( -- * Constants
      devnetMagic
    , genesisDir

      -- * Genesis key
    , genesisSignKey
    , genesisAddr

      -- * Key generation
    , mkSignKey
    , keyHashFromSignKey
    , enterpriseAddr

      -- * Signing
    , addKeyWitness

      -- * Devnet bracket
    , withDevnet
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( async
    , cancel
    , poll
    )
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import System.Environment (lookupEnv)

import Cardano.Crypto.DSIGN
    ( Ed25519DSIGN
    , SignKeyDSIGN
    , deriveVerKeyDSIGN
    , genKeyDSIGN
    )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api.Tx
    ( Tx
    , addrTxWitsL
    , txIdTx
    , witsTxL
    )
import Cardano.Ledger.Api.Tx.In (TxId (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (extractHash)
import Cardano.Ledger.Credential
    ( Credential (..)
    , StakeReference (..)
    )
import Cardano.Ledger.Keys
    ( KeyHash
    , KeyRole (..)
    , VKey (..)
    , WitVKey (..)
    , asWitness
    , hashKey
    , signedDSIGN
    )
import Lens.Micro ((%~), (&))
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )

import Cardano.Node.Client.E2E.Devnet
    ( withCardanoNode
    )
import Cardano.Node.Client.N2C.Connection
    ( newLSQChannel
    , newLTxSChannel
    , runNodeClient
    )
import Cardano.Node.Client.N2C.Types
    ( LSQChannel
    , LTxSChannel
    )

-- | Devnet uses network magic 42.
devnetMagic :: NetworkMagic
devnetMagic = NetworkMagic 42

{- | Path to the checked-in genesis directory.
Override with @E2E_GENESIS_DIR@ env var.
-}
genesisDir :: IO FilePath
genesisDir = do
    mPath <- lookupEnv "E2E_GENESIS_DIR"
    pure
        $ fromMaybe
            "integration-test/genesis"
            mPath

{- | Genesis UTxO signing key. Matches the
address in @shelley-genesis.json@
@initialFunds@.
Seed must be exactly 32 bytes.
-}
genesisSignKey :: SignKeyDSIGN Ed25519DSIGN
genesisSignKey =
    mkSignKey
        "e2e-genesis-utxo-key-seed-000001"

{- | Enterprise testnet address for the genesis
UTxO key.
-}
genesisAddr :: Addr
genesisAddr =
    enterpriseAddr
        (keyHashFromSignKey genesisSignKey)

{- | Derive an Ed25519 signing key from a
32-byte seed.
-}
mkSignKey
    :: ByteString
    -> SignKeyDSIGN Ed25519DSIGN
mkSignKey seed =
    genKeyDSIGN (mkSeedFromBytes seed)

{- | Derive the payment key hash from a signing
key via 'VKey' + 'hashKey'.
-}
keyHashFromSignKey
    :: SignKeyDSIGN Ed25519DSIGN
    -> KeyHash 'Payment
keyHashFromSignKey sk =
    hashKey (VKey (deriveVerKeyDSIGN sk))

{- | Enterprise testnet address from a payment
key hash.
-}
enterpriseAddr :: KeyHash 'Payment -> Addr
enterpriseAddr kh =
    Addr Testnet (KeyHashObj kh) StakeRefNull

-- | Add a key witness to a transaction.
addKeyWitness
    :: SignKeyDSIGN Ed25519DSIGN
    -> Tx ConwayEra
    -> Tx ConwayEra
addKeyWitness sk tx =
    tx & witsTxL . addrTxWitsL %~ Set.union wits
  where
    wits =
        Set.singleton (mkWitVKey (txIdTx tx) sk)

{- | Create a 'WitVKey' from a 'TxId' and
signing key.
-}
mkWitVKey
    :: TxId
    -> SignKeyDSIGN Ed25519DSIGN
    -> WitVKey 'Witness
mkWitVKey (TxId hash) sk =
    WitVKey
        (asWitness vk)
        (signedDSIGN sk (extractHash hash))
  where
    vk = VKey (deriveVerKeyDSIGN sk)

{- | Start a cardano-node devnet, connect via
N2C, run an action, and tear down.
-}
withDevnet
    :: (LSQChannel -> LTxSChannel -> IO a)
    -> IO a
withDevnet action = do
    gDir <- genesisDir
    withCardanoNode gDir $ \sock _startMs -> do
        lsqCh <- newLSQChannel 16
        ltxsCh <- newLTxSChannel 16
        nodeThread <-
            async
                $ runNodeClient
                    devnetMagic
                    sock
                    lsqCh
                    ltxsCh
        threadDelay 3_000_000
        status <- poll nodeThread
        case status of
            Just (Left err) ->
                error
                    $ "Node connection failed: "
                        <> show err
            Just (Right (Left err)) ->
                error
                    $ "Node connection error: "
                        <> show err
            Just (Right (Right ())) ->
                error
                    "Node connection closed \
                    \unexpectedly"
            Nothing -> pure ()
        result <- action lsqCh ltxsCh
        cancel nodeThread
        pure result
