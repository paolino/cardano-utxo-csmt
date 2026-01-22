module Cardano.UTxOCSMT.HTTP.Server
    ( runAPIServer
    , runDocsServer
    , apiApp
    , docsApp
    )
where

import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Cardano.UTxOCSMT.HTTP.API (API, api, docs)
import Cardano.UTxOCSMT.HTTP.Swagger (swaggerServer)
import Control.Monad.IO.Class (MonadIO (..))
import Network.Socket (PortNumber)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Server, serve)

-- | Server implementation for the API
apiServer :: IO (Maybe Metrics) -> Server API
apiServer getMetrics = do
    r <- liftIO getMetrics
    case r of
        Just metrics -> return metrics
        Nothing -> fail "Metrics not yet available"

-- | WAI Application for the API
apiApp :: IO (Maybe Metrics) -> Application
apiApp getMetrics = serve api $ apiServer getMetrics

-- | Run the API server on the specified port
-- Takes a port number and an IO action that provides the current Metrics
runAPIServer :: PortNumber -> IO (Maybe Metrics) -> IO ()
runAPIServer port getMetrics = run (fromIntegral port) $ apiApp getMetrics

-- | WAI Application for the documentation
docsApp :: Application
docsApp = serve docs swaggerServer

-- | Run the documentation server on the specified port
runDocsServer :: PortNumber -> IO ()
runDocsServer port = run (fromIntegral port) docsApp
