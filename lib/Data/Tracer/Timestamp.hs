module Data.Tracer.Timestamp
    ( Timestamp (..)
    , timestamp
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Tracer (Tracer (..), traceWith)
import Data.Time.Clock (UTCTime, getCurrentTime)

-- | Event with timestamp
data Timestamp a = Timestamp
    { timestampTime :: UTCTime
    , timestampEvent :: a
    }

-- | Add timestamps to events
timestamp
    :: MonadIO m
    => Tracer m (Timestamp a)
    -> Tracer m a
timestamp baseTracer = Tracer $ \event -> do
    now <- liftIO getCurrentTime
    traceWith baseTracer $ Timestamp now event
