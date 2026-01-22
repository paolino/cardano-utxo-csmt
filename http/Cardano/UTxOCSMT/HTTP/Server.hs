module Cardano.UTxOCSMT.HTTP.Server
    ( runAPIServer
    , runDocsServer
    , apiApp
    , docsApp
    )
where

import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Cardano.UTxOCSMT.HTTP.API (API, MerkleRootEntry, api, docs)
import Cardano.UTxOCSMT.HTTP.Swagger (swaggerServer)
import Control.Monad.IO.Class (MonadIO (..))
import Network.Socket (PortNumber)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (Server, serve, (:<|>) (..))

-- | Server implementation for the API
apiServer :: IO (Maybe Metrics) -> IO [MerkleRootEntry] -> Server API
apiServer getMetrics getMerkleRoots = metricsHandler :<|> merkleRootsHandler
  where
    metricsHandler = do
        r <- liftIO getMetrics
        case r of
            Just metrics -> return metrics
            Nothing -> fail "Metrics not yet available"

    merkleRootsHandler = liftIO getMerkleRoots

-- | WAI Application for the API
apiApp :: IO (Maybe Metrics) -> IO [MerkleRootEntry] -> Application
apiApp getMetrics getMerkleRoots = simpleCors $ serve api $ apiServer getMetrics getMerkleRoots

-- | Run the API server on the specified port
-- Takes a port number, an IO action that provides the current Metrics,
-- and an IO action that retrieves all merkle roots
runAPIServer :: PortNumber -> IO (Maybe Metrics) -> IO [MerkleRootEntry] -> IO ()
runAPIServer port getMetrics getMerkleRoots =
    run (fromIntegral port) $ apiApp getMetrics getMerkleRoots

-- | WAI Application for the documentation
docsApp :: Maybe PortNumber -> Application
docsApp mApiPort = serve docs (swaggerServer mApiPort)

-- | Run the documentation server on the specified port
-- Takes the docs port and optionally the API port (for Swagger to point to)
runDocsServer :: PortNumber -> Maybe PortNumber -> IO ()
runDocsServer port mApiPort = run (fromIntegral port) (docsApp mApiPort)
