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
    , ReadyResponse (..)
    , api
    , docs
    )
import Cardano.UTxOCSMT.HTTP.Swagger (swaggerServer)
import Control.Monad (unless)
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
    , err503
    , serve
    , throwError
    , (:<|>) (..)
    )

-- | Server implementation for the API
apiServer
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> IO ReadyResponse
    -> Server API
apiServer getMetrics getMerkleRoots getProof getReady =
    metricsHandler
        :<|> merkleRootsHandler
        :<|> proofHandler
        :<|> readyHandler
  where
    metricsHandler = do
        r <- liftIO getMetrics
        maybe (throwError err404) return r

    -- \| Check sync status and return 503 if not ready
    requireSynced :: Handler a -> Handler a
    requireSynced action = do
        ReadyResponse{ready} <- liftIO getReady
        unless ready $ throwError err503
        action

    merkleRootsHandler = requireSynced $ liftIO getMerkleRoots

    proofHandler txId txIx = requireSynced $ do
        r <- liftIO $ getProof txId txIx
        maybe
            (throwError err404)
            (pure :: InclusionProofResponse -> Handler InclusionProofResponse)
            r

    readyHandler = liftIO getReady

-- | WAI Application for the API
apiApp
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> IO ReadyResponse
    -> Application
apiApp getMetrics getMerkleRoots getProof getReady =
    simpleCors
        $ serve api
        $ apiServer getMetrics getMerkleRoots getProof getReady

{- | Run the API server on the specified port
Takes a port number, an IO action that provides the current Metrics,
an IO action that retrieves all merkle roots, a proof query function,
and an IO action that returns the sync readiness status
-}
runAPIServer
    :: PortNumber
    -> IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> IO ReadyResponse
    -> IO ()
runAPIServer port getMetrics getMerkleRoots getProof getReady =
    run (fromIntegral port)
        $ apiApp getMetrics getMerkleRoots getProof getReady

-- | WAI Application for the documentation
docsApp :: Maybe PortNumber -> Application
docsApp mApiPort = serve docs (swaggerServer mApiPort)

{- | Run the documentation server on the specified port
Takes the docs port and optionally the API port (for Swagger to point to)
-}
runDocsServer :: PortNumber -> Maybe PortNumber -> IO ()
runDocsServer port mApiPort = run (fromIntegral port) (docsApp mApiPort)
