{-# LANGUAGE OverloadedStrings #-}

module Cardano.UTxOCSMT.HTTP.Swagger
    ( swaggerDoc
    , swaggerServer
    , SwaggerAPI
    , renderSwaggerJSON
    )
where

import Cardano.UTxOCSMT.HTTP.API (api)
import Control.Lens ((&), (.~), (?~))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (LazyByteString)
import Data.Swagger
    ( Host (..)
    , Swagger
    , description
    , host
    , info
    , license
    , title
    , version
    )
import Network.Socket (PortNumber)
import Servant (Server)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI)
import Servant.Swagger.UI qualified as SwaggerUI

-- | Generate Swagger documentation for the API
swaggerDoc :: Maybe Host -> Swagger
swaggerDoc mHost =
    toSwagger api
        & info . title .~ "Cardano UTxO CSMT API"
        & info . version .~ "0.1.0.0"
        & info . description
            ?~ "An HTTP service that exposes a Compact Sparse Merkle Tree (CSMT) \
               \data structure over the current UTxO set of the Cardano blockchain. \
               \This API provides metrics about the CSMT operations."
        & info . license ?~ "Apache 2.0"
        & host .~ mHost

-- | Type alias for Swagger UI with schema
type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | Servant server for Swagger UI
swaggerServer :: Maybe PortNumber -> Server SwaggerAPI
swaggerServer mApiPort = SwaggerUI.swaggerSchemaUIServer (swaggerDoc mHost)
  where
    mHost =
        fmap
            (\p -> Host{_hostName = "localhost", _hostPort = Just p})
            mApiPort

-- | Render Swagger documentation as JSON
renderSwaggerJSON :: LazyByteString
renderSwaggerJSON = encodePretty $ swaggerDoc Nothing