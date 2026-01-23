module Cardano.UTxOCSMT.HTTP.API
    ( api
    , API
    , DOCS
    , docs
    , MerkleRootEntry (..)
    , InclusionProofResponse (..)
    )
where

import CSMT.Hashes (Hash, renderHash)
import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Control.Lens ((&), (.~), (?~))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.Proxy (Proxy (..))
import Data.Swagger
    ( ToSchema (..)
    , declareSchemaRef
    , description
    , properties
    , required
    )
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word16)
import GHC.IsList (IsList (..))
import Ouroboros.Network.Block (SlotNo (..))
import Servant (Capture, Get, JSON, type (:<|>), type (:>))
import Servant.Swagger.UI (SwaggerSchemaUI)

-- | Type alias for API documentation endpoint
type DOCS = "api-docs" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | Proxy for API documentation endpoint
docs :: Proxy DOCS
docs = Proxy

-- | Type alias for the API
type API =
    "metrics"
        :> Get '[JSON] Metrics
        :<|> "merkle-roots"
            :> Get '[JSON] [MerkleRootEntry]
        :<|> "proof"
            :> Capture "txId" Text
            :> Capture "txIx" Word16
            :> Get '[JSON] InclusionProofResponse

-- | Proxy for the API
api :: Proxy API
api = Proxy

-- | Entry for a merkle root at a given slot
data MerkleRootEntry = MerkleRootEntry
    { slotNo :: SlotNo
    , blockHash :: Hash
    , merkleRoot :: Maybe Hash
    }
    deriving (Show, Eq)

data InclusionProofResponse = InclusionProofResponse
    { proofTxId :: Text
    , proofTxIx :: Word16
    , proofTxOut :: Text
    , proofBytes :: Text
    , proofMerkleRoot :: Maybe Text
    }
    deriving (Show, Eq)

renderHashBase16 :: Hash -> Text
renderHashBase16 = Text.decodeUtf8 . convertToBase Base16 . renderHash

instance ToJSON MerkleRootEntry where
    toJSON MerkleRootEntry{slotNo, blockHash, merkleRoot} =
        object
            [ "slotNo" .= unSlotNo slotNo
            , "blockHash" .= renderHashBase16 blockHash
            , "merkleRoot" .= fmap renderHashBase16 merkleRoot
            ]

instance ToJSON InclusionProofResponse where
    toJSON
        InclusionProofResponse
            { proofTxId
            , proofTxIx
            , proofTxOut
            , proofBytes
            , proofMerkleRoot
            } =
            object
                [ "txId" .= proofTxId
                , "txIx" .= proofTxIx
                , "txOut" .= proofTxOut
                , "proof" .= proofBytes
                , "merkleRoot" .= proofMerkleRoot
                ]

instance ToSchema MerkleRootEntry where
    declareNamedSchema _ = do
        word64Schema <- declareSchemaRef (Proxy @Word)
        stringSchema <- declareSchemaRef (Proxy @String)
        maybeStringSchema <- declareSchemaRef (Proxy @(Maybe String))
        return
            $ Swagger.NamedSchema (Just "MerkleRootEntry")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("slotNo", word64Schema)
                    , ("blockHash", stringSchema)
                    , ("merkleRoot", maybeStringSchema)
                    ]
            & required .~ ["slotNo", "blockHash", "merkleRoot"]
            & description ?~ "A merkle root entry at a given slot"

instance ToSchema InclusionProofResponse where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @String)
        word16Schema <- declareSchemaRef (Proxy @Word16)
        maybeStringSchema <- declareSchemaRef (Proxy @(Maybe String))
        return
            $ Swagger.NamedSchema (Just "InclusionProofResponse")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("txId", stringSchema)
                    , ("txIx", word16Schema)
                    , ("txOut", stringSchema)
                    , ("proof", stringSchema)
                    , ("merkleRoot", maybeStringSchema)
                    ]
            & required .~ ["txId", "txIx", "txOut", "proof"]
            & description
                ?~ "Current inclusion proof and output for the given transaction input."
