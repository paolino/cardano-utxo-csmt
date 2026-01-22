module Data.Tracer.ThreadSafe (newThreadSafeTracer)
where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Tracer (Tracer (..), traceWith)

-- | Create a thread-safe tracer by synchronizing access to the underlying tracer.
newThreadSafeTracer
    :: Tracer IO a
    -- ^ The underlying unsafe tracer
    -> IO (Tracer IO a)
newThreadSafeTracer unsafe = do
    mvar <- newMVar ()
    return $ Tracer $ \a ->
        withMVar mvar $ \_ ->
            traceWith unsafe a
