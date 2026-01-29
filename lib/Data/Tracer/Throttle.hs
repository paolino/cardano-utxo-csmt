{-# LANGUAGE NamedFieldPuns #-}

module Data.Tracer.Throttle
    ( Throttled (..)
    , throttleByFrequency
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Tracer (Tracer (..), traceWith)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Tracer.Timestamp (Timestamp (..))
import Data.Word (Word64)

-- | Throttled event with drop count
data Throttled a = Throttled
    { throttledDropped :: Word64
    , throttledEvent :: a
    }

{- | Create a throttled tracer

Events matching a frequency constraint are emitted at most every
@1/freq@ seconds. Non-matching events pass through immediately.
The @Throttled@ wrapper includes the count of dropped events
since the last emission.

Uses the timestamp from the input event for timing calculations.
-}
throttleByFrequency
    :: MonadIO m
    => [a -> Maybe Double]
    -- ^ matchers on raw event, returning max frequency if matched
    -> Tracer m (Throttled (Timestamp a))
    -- ^ output tracer wraps timestamped event
    -> m (Tracer m (Timestamp a))
    -- ^ input tracer receives timestamped events
throttleByFrequency matchers baseTracer = do
    stateRef <- liftIO $ newIORef Map.empty
    pure $ Tracer $ \ts@Timestamp{timestampTime, timestampEvent} -> do
        case findMatcher matchers timestampEvent of
            Nothing ->
                traceWith baseTracer $ Throttled 0 ts
            Just (idx, freq) -> do
                let interval = 1.0 / freq
                state <- liftIO $ readIORef stateRef
                let (lastTime, dropped) =
                        Map.findWithDefault (epoch, 0) idx state
                    elapsed =
                        realToFrac
                            $ diffUTCTime timestampTime lastTime
                if elapsed >= interval
                    then do
                        liftIO
                            $ writeIORef stateRef
                            $ Map.insert idx (timestampTime, 0) state
                        traceWith baseTracer $ Throttled dropped ts
                    else
                        liftIO
                            $ writeIORef stateRef
                            $ Map.insert
                                idx
                                (lastTime, dropped + 1)
                                state
  where
    epoch :: UTCTime
    epoch = read "1970-01-01 00:00:00 UTC"

    findMatcher :: [a -> Maybe Double] -> a -> Maybe (Int, Double)
    findMatcher fs x = go 0 fs
      where
        go _ [] = Nothing
        go i (f : rest) = case f x of
            Just freq -> Just (i, freq)
            Nothing -> go (i + 1) rest
