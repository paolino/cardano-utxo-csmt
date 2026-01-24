{- |
Module      : Cardano.UTxOCSMT.Application.UTxOs
Description : UTxO extraction from Cardano blocks

This module extracts UTxO changes (spends and creates) from Cardano blocks
across all eras (Byron through Conway). Each transaction produces:

* 'Spend' entries for consumed inputs
* 'Create' entries for new outputs

UTxO references are CBOR-encoded for consistent storage and lookup.
-}
module Cardano.UTxOCSMT.Application.UTxOs
    ( uTxOs
    , Change (..)
    , mkShelleyTxIn
    , unsafeMkTxIn
    )
where

import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash.Class (Hash (..))
import Cardano.Ledger.Api.Tx.In (mkTxIxPartial)
import Cardano.Ledger.Api.Tx.In qualified as Shelley
import Cardano.Ledger.Binary (EncCBOR, natVersion, serialize)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Read.Ledger.Block.Block (fromConsensusBlock)
import Cardano.Read.Ledger.Block.Txs (getEraTransactions)
import Cardano.Read.Ledger.Eras.EraValue (applyEraFun)
import Cardano.Read.Ledger.Eras.KnownEras
    ( Era (..)
    , IsEra
    , theEra
    )
import Cardano.Read.Ledger.Tx.Inputs (Inputs (..), getEraInputs)
import Cardano.Read.Ledger.Tx.Outputs (Outputs (..), getEraOutputs)
import Cardano.Read.Ledger.Tx.Tx (Tx (..))
import Cardano.Read.Ledger.Tx.TxId (TxId (..), getEraTxId)
import Cardano.UTxOCSMT.Ouroboros.Types (Block)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (toList)
import Data.Word (Word16)

data Change = Spend ByteString | Create ByteString ByteString
    deriving (Eq)

viewBinary :: BL.LazyByteString -> String
viewBinary = B.unpack . convertToBase Base64 . BL.toStrict

instance Show Change where
    show (Spend txIn) = "d " ++ viewBinary txIn
    show (Create txIn txOut) =
        "i "
            ++ viewBinary txIn
            ++ " "
            ++ viewBinary txOut

uTxOs :: Block -> [Change]
uTxOs bl =
    (changes . getEraTransactions) `applyEraFun` fromConsensusBlock bl

changes :: IsEra era => [Tx era] -> [Change]
changes = concatMap $ \tx ->
    let inputs = Spend <$> extractInputs (getEraInputs tx)
        outputs = zipWith (mkCreate tx) [0 ..] (extractOutputs (getEraOutputs tx))
    in  inputs ++ outputs

mkCreate :: IsEra era => Tx era -> Word16 -> ByteString -> Change
mkCreate tx index = Create (mkTxIn tx index)

unsafeMkTxId :: ShortByteString -> Shelley.TxId
unsafeMkTxId = Shelley.TxId . unsafeMakeSafeHash . UnsafeHash

unsafeMkTxIn :: ShortByteString -> Word16 -> ByteString
unsafeMkTxIn txId index = mkShelleyTxIn index (unsafeMkTxId txId)

{-# INLINEABLE mkTxIn #-}
mkTxIn :: forall era. IsEra era => Tx era -> Word16 -> ByteString
mkTxIn tx index = case theEra @era of
    Byron -> cborEncode $ Byron.TxInUtxo (unTxId $ getEraTxId tx) index
    Shelley -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Allegra -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Mary -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Alonzo -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Babbage -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Conway -> mkShelleyTxIn index $ unTxId $ getEraTxId tx

mkShelleyTxIn :: Word16 -> Shelley.TxId -> ByteString
mkShelleyTxIn index h =
    cborEncode
        $ Shelley.TxIn h
        $ mkTxIxPartial
        $ fromIntegral index

{-# INLINEABLE extractInputs #-}
extractInputs :: forall era. IsEra era => Inputs era -> [ByteString]
extractInputs (Inputs ins) = case theEra @era of
    Byron -> cborEncode <$> toList ins
    Shelley -> cborEncode <$> toList ins
    Allegra -> cborEncode <$> toList ins
    Mary -> cborEncode <$> toList ins
    Alonzo -> cborEncode <$> toList ins
    Babbage -> cborEncode <$> toList ins
    Conway -> cborEncode <$> toList ins

cborEncode :: EncCBOR a => a -> ByteString
cborEncode = serialize (natVersion @11)

_txs :: forall era. IsEra era => Tx era -> ByteString
_txs (Tx tx) = case theEra @era of
    Byron -> cborEncode tx
    Shelley -> cborEncode tx
    Allegra -> cborEncode tx
    Mary -> cborEncode tx
    Alonzo -> cborEncode tx
    Babbage -> cborEncode tx
    Conway -> cborEncode tx

{-# INLINEABLE extractOutputs #-}
extractOutputs :: forall era. IsEra era => Outputs era -> [ByteString]
extractOutputs (Outputs outs) = case theEra @era of
    Byron -> cborEncode <$> toList outs
    Shelley -> cborEncode <$> toList outs
    Allegra -> cborEncode <$> toList outs
    Mary -> cborEncode <$> toList outs
    Alonzo -> cborEncode <$> toList outs
    Babbage -> cborEncode <$> toList outs
    Conway -> cborEncode <$> toList outs
