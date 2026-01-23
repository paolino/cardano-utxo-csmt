module Cardano.UTxOCSMT.HTTP.Server
    ( runAPIServer
    , runDocsServer
    , apiApp
    , docsApp
    )
where

import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Cardano.UTxOCSMT.HTTP.API
    ( API
    , InclusionProofResponse
    , MerkleRootEntry
    , api
    , docs
    )
import Cardano.UTxOCSMT.HTTP.Swagger (swaggerServer)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Word (Word16)
import Network.Socket (PortNumber)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
    ( Handler
    , Server
    , err404
    , serve
    , throwError
    , (:<|>) (..)
    )

-- | Server implementation for the API
apiServer
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> Server API
apiServer getMetrics getMerkleRoots getProof =
    metricsHandler :<|> merkleRootsHandler :<|> proofHandler
  where
    metricsHandler = do
        r <- liftIO getMetrics
        case r of
            Just metrics -> return metrics
            Nothing -> fail "Metrics not yet available"

    merkleRootsHandler = liftIO getMerkleRoots

    proofHandler txId txIx = do
        r <- liftIO $ getProof txId txIx
        maybe
            (throwError err404)
            (pure :: InclusionProofResponse -> Handler InclusionProofResponse)
            r

-- | WAI Application for the API
apiApp
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> Application
apiApp getMetrics getMerkleRoots getProof =
    simpleCors $ serve api $ apiServer getMetrics getMerkleRoots getProof

-- | Run the API server on the specified port
-- Takes a port number, an IO action that provides the current Metrics,
-- and an IO action that retrieves all merkle roots
runAPIServer
    :: PortNumber
    -> IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> IO ()
runAPIServer port getMetrics getMerkleRoots getProof =
    run (fromIntegral port) $ apiApp getMetrics getMerkleRoots getProof

-- | WAI Application for the documentation
docsApp :: Maybe PortNumber -> Application
docsApp mApiPort = serve docs (swaggerServer mApiPort)

-- | Run the documentation server on the specified port
-- Takes the docs port and optionally the API port (for Swagger to point to)
runDocsServer :: PortNumber -> Maybe PortNumber -> IO ()
runDocsServer port mApiPort = run (fromIntegral port) (docsApp mApiPort)
