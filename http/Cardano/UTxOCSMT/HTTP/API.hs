module Cardano.UTxOCSMT.HTTP.API
    ( api
    , API
    , DOCS
    , docs
    )
where

import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Servant (Get, JSON, Proxy (..), type (:>))
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

-- | Proxy for the API
api :: Proxy API
api = Proxy