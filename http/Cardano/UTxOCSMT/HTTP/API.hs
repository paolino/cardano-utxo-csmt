module Cardano.UTxOCSMT.HTTP.API
    ( api
    , API
    , DOCS
    , docs
    , MerkleRootEntry (..)
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
import GHC.IsList (IsList (..))
import Ouroboros.Network.Block (SlotNo (..))
import Servant (Get, JSON, type (:<|>), type (:>))
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

renderHashBase16 :: Hash -> Text
renderHashBase16 = Text.decodeUtf8 . convertToBase Base16 . renderHash

instance ToJSON MerkleRootEntry where
    toJSON MerkleRootEntry{slotNo, blockHash, merkleRoot} =
        object
            [ "slotNo" .= unSlotNo slotNo
            , "blockHash" .= renderHashBase16 blockHash
            , "merkleRoot" .= fmap renderHashBase16 merkleRoot
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
