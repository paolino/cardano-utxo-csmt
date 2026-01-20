module Data.Tracer.Timestamps
    ( addTimestampsTracer
    )
where

import Control.Tracer (Tracer (..))
import Data.Time.Clock (getCurrentTime)

-- | Prepend timestamps to log messages.
addTimestampsTracer :: Tracer IO String -> Tracer IO String
addTimestampsTracer (Tracer f) = Tracer $ \msg -> do
    time <- getCurrentTime
    f $ "[" ++ show time ++ "] " ++ msg
