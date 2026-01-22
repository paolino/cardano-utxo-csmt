{-# LANGUAGE OverloadedStrings #-}

module Cardano.UTxOCSMT.HTTP.Swagger
    ( swaggerDoc
    , swaggerServer
    , SwaggerAPI
    )
where

import Cardano.UTxOCSMT.HTTP.API (api)
import Control.Lens ((&), (.~), (?~))
import Data.Swagger
    ( Swagger
    , description
    , info
    , license
    , title
    , version
    )
import Servant (Server)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI)
import Servant.Swagger.UI qualified as SwaggerUI

-- | Generate Swagger documentation for the API
swaggerDoc :: Swagger
swaggerDoc =
    toSwagger api
        & info . title .~ "Cardano UTxO CSMT API"
        & info . version .~ "0.1.0.0"
        & info . description
            ?~ "An HTTP service that exposes a Compact Sparse Merkle Tree (CSMT) \
               \data structure over the current UTxO set of the Cardano blockchain. \
               \This API provides metrics about the CSMT operations."
        & info . license ?~ "Apache 2.0"

-- | Type alias for Swagger UI with schema
type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | Servant server for Swagger UI
swaggerServer :: Server SwaggerAPI
swaggerServer = SwaggerUI.swaggerSchemaUIServer swaggerDoc
