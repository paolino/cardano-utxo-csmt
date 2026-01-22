{-# LANGUAGE OverloadedStrings #-}

module Cardano.UTxOCSMT.HTTP.Server
    ( runAPIServer
    , runDocsServer
    , apiApp
    , docsApp
    )
where

import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Cardano.UTxOCSMT.HTTP.API (API, DOCS, api, docs)
import Cardano.UTxOCSMT.HTTP.Swagger (swaggerServer)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, run)
import Servant (Proxy (..), Server, serve)

-- | Server implementation for the API
apiServer :: IO Metrics -> Server API
apiServer getMetrics = getMetrics

-- | WAI Application for the API
apiApp :: IO Metrics -> Application
apiApp getMetrics = serve api (apiServer getMetrics)

-- | Run the API server on the specified port
-- Takes a port number and an IO action that provides the current Metrics
runAPIServer :: Port -> IO Metrics -> IO ()
runAPIServer port getMetrics = run port (apiApp getMetrics)

-- | WAI Application for the documentation
docsApp :: Application
docsApp = serve docs swaggerServer

-- | Run the documentation server on the specified port
runDocsServer :: Port -> IO ()
runDocsServer port = run port docsApp
