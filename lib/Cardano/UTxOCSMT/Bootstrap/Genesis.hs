{- |
Module      : Cardano.UTxOCSMT.Bootstrap.Genesis
Description : Genesis UTxO bootstrap for from-genesis sync

Parse a @shelley-genesis.json@ file and extract the @initialFunds@
as CBOR-encoded (TxIn, TxOut) pairs ready for CSMT insertion.
This allows syncing from genesis without Mithril by pre-populating
the tree with genesis UTxOs before chain sync starts.
-}
module Cardano.UTxOCSMT.Bootstrap.Genesis
    ( readShelleyGenesis
    , genesisUtxoPairs
    )
where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Api.Tx.Out (mkBasicTxOut)
import Cardano.Ledger.Binary (natVersion, serialize)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.Genesis
    ( ShelleyGenesis (..)
    , initialFundsPseudoTxIn
    )
import Cardano.Ledger.Val (inject)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ListMap (unListMap)

{- | Read and parse a Shelley genesis JSON file.
Patches @systemStart@ if set to @\"PLACEHOLDER\"@.
-}
readShelleyGenesis :: FilePath -> IO ShelleyGenesis
readShelleyGenesis fp = do
    raw <- BSL.readFile fp
    val <- case Aeson.eitherDecode' raw of
        Left err ->
            error $ "Genesis JSON parse error: " <> err
        Right v -> pure (patchSystemStart v)
    case Aeson.fromJSON val of
        Aeson.Error err ->
            error $ "Genesis decode error: " <> err
        Aeson.Success g -> pure g

{- | Extract genesis UTxO pairs as CBOR-encoded (TxIn, TxOut).
Each initial fund entry becomes a pseudo-TxIn (derived from the
address hash) paired with a Conway-era TxOut.
-}
genesisUtxoPairs
    :: ShelleyGenesis -> [(ByteString, ByteString)]
genesisUtxoPairs genesis =
    mkPair <$> unListMap (sgInitialFunds genesis)
  where
    ver = natVersion @11
    mkPair :: (Addr, Coin) -> (ByteString, ByteString)
    mkPair (addr, coin) =
        let txIn = initialFundsPseudoTxIn addr
            txOut =
                mkBasicTxOut @ConwayEra addr (inject coin)
        in  (serialize ver txIn, serialize ver txOut)

{- | Replace @\"PLACEHOLDER\"@ systemStart with a dummy UTC time
so that 'FromJSON' succeeds.
-}
patchSystemStart :: Aeson.Value -> Aeson.Value
patchSystemStart = \case
    Aeson.Object obj
        | Just (Aeson.String "PLACEHOLDER") <-
            KM.lookup "systemStart" obj ->
            Aeson.Object
                $ KM.insert
                    "systemStart"
                    (Aeson.String "2020-01-01T00:00:00Z")
                    obj
    v -> v
